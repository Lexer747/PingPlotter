import Data.Bits
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.CPUTime
import Network.Socket (Family(AF_INET), Socket, SocketType(Raw), SockAddr(SockAddrInet),addrAddress,addrFamily, addrProtocol, addrSocketType, ProtocolNumber, connect,isConnected, getAddrInfo, socket, close, withSocketsDo)
import Network.Socket.ByteString (sendTo, recvFrom)
import System.Timeout (timeout)

newtype TTL = TTL Word8
newtype PacketType = PacketType Word8 deriving Show
newtype Code = Code Word8 deriving Show
newtype CheckSum = CheckSum Word16 deriving Show
newtype Identifier = Identifier Word16 deriving (Eq, Show)
newtype Sequence = Sequence Word16 deriving Show
newtype ICMPData = ICMPData Word64 deriving Show

timeoutDuration = 10^6
maxReceive = 2048
ipHeaderLength = 20
icmpHeaderLength = 8

data ICMPHeader = ICMPHeader PacketType Code CheckSum Identifier Sequence

getICMPHeader :: Get ICMPHeader
getICMPHeader = do
  gType <- getWord8
  gCode <- getWord8
  gChecksum <- getWord16be
  gIdentifier <- getWord16be
  gSequence <- getWord16be
  return $ ICMPHeader (PacketType gType) (Code gCode) (CheckSum gChecksum) (Identifier gIdentifier) (Sequence gSequence)

getICMPData :: Get ICMPData
getICMPData = ICMPData <$> getWord64be

getTtl :: Get TTL
getTtl = TTL <$> getWord8
  
data ICMPRequest = ICMPRequest {
    packetType :: PacketType,
    code :: Code,
    checkSum :: CheckSum,
    id_ :: Identifier,
    sequence__ :: Sequence,
    timeStamp :: ICMPData
} deriving Show

getTime :: IO Word64
getTime = do
    t <- getCPUTime
    return $ fromIntegral $ t `div` 100000


writeToBuffer :: ICMPRequest -> Put
writeToBuffer icmp = do
    let
        (PacketType pt) = packetType icmp
        (Code co) = code icmp
        (CheckSum ch) = checkSum icmp
        (Identifier i) = id_ icmp
        (Sequence s) = sequence__ icmp
        (ICMPData d) = timeStamp icmp
    putWord8 $ pt
    putWord8 $ co
    putWord16be $ ch
    putWord16be $ i
    putWord16be $ s
    putWord64be $ d
  
buildRequest :: Identifier -> Sequence -> ICMPData -> ICMPRequest
buildRequest identifier sequence__ timeStamp = ICMPRequest (PacketType 8) (Code 0) (CheckSum checkSum) identifier sequence__ timeStamp
    where
        --build an empty packet so we can calc a check sum
        initialPacket = ICMPRequest (PacketType 8) (Code 0) (CheckSum 0) identifier sequence__ timeStamp
        
        -- "If the total length is odd, the received data is padded with one
        -- octet of zeros for computing the checksum." - RFC 792
        maybeAddOctet bs
            | (BL.length bs) `mod` 2 == 0 = bs
            | otherwise                   = BL.snoc bs 0
            
        --split packet into 16 bit word list
        splitBuffer :: Get [Word16]
        splitBuffer = do
            empty <- isEmpty
            if empty then return []
                else do
                    w16 <- getWord16be
                    rest <- splitBuffer
                    return (w16 : rest)
         
         --make the packet into computable types
        values :: (Num a) => [a]
        values = Prelude.map fromIntegral $ runGet splitBuffer ((maybeAddOctet . runPut . writeToBuffer) initialPacket)
            
        --sum to 32 bit
        total :: Word32
        total = sum values
         
        --2's complement system, so we have to convert into 1's complement
        low = fromIntegral $ total :: Word16 --sum without carries
        high = fromIntegral $ (total `shiftR` 16) :: Word16
        eac = low + high --end around carry
        
          
        checkSum :: Word16
        checkSum = complement $ eac

listenForReply :: Int -> Socket -> SockAddr -> Identifier -> Sequence -> IO ()
listenForReply bytesSent s sa id_ (Sequence sequence__) = withSocketsDo $ do
    response <- timeout timeoutDuration (recvFrom s maxReceive)
    case response of
        Just ((reply, senderAddress)) -> do
            receivedAt <- getTime
            let
                (ipHeader, ipData) = B.splitAt ipHeaderLength reply
                (_, ttlAndRest) = B.splitAt 8 ipHeader
                (TTL timeToLive) = runGet getTtl (BL.fromStrict ttlAndRest)
                (icmpHeader, icmpData) = B.splitAt icmpHeaderLength  ipData
                (ICMPHeader _ _ _ replyID _) = runGet getICMPHeader (BL.fromStrict icmpHeader)
                (ICMPData timestamp) = runGet getICMPData (BL.fromStrict icmpData)
            if ((replyID == id_) && sa == senderAddress)
            then do
                let 
                    ttl = fromIntegral timeToLive
                    sentAt = fromIntegral timestamp
                    delta = (fromIntegral receivedAt) - sentAt
                Prelude.putStrLn $ " time: " ++ (show delta)
                pingHost s sa id_ (Sequence (sequence__ + 1))
            else do
                Prelude.putStrLn $ "invalid packet"
                listenForReply bytesSent s sa id_ (Sequence sequence__)
        Nothing -> do
            Prelude.putStrLn $ "timed out"
            pingHost s sa id_ (Sequence (sequence__ + 1))
                

pingHost :: Socket -> SockAddr -> Identifier -> Sequence -> IO ()
pingHost s sa id_ sequence__ = withSocketsDo $ do
     t <- getTime
     bytesSent <- sendTo s ((B.concat . BL.toChunks . runPut . writeToBuffer) $ buildRequest id_ sequence__ (ICMPData t)) sa
     listenForReply bytesSent s sa id_ sequence__
    
ping :: IO ()
ping = do
    