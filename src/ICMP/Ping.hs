import Data.Bits
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

newtype Type = Type Word8 deriving Show
newtype Code = Code Word8 deriving Show
newtype CheckSum = CheckSum Word16 deriving Show
newtype Indentifier = Indentifier Word16 deriving Show
newtype Sequence = Sequence Word16 deriving Show
newtype ICMPData = ICMPData Word64 deriving Show

data ICMPRequest = ICMPRequest {
    packetType :: Type,
    code :: Code,
    checkSum :: CheckSum,
    id_ :: Indentifier,
    sequence__ :: Sequence,
    timeStamp :: ICMPData
}


writeToBuffer :: ICMPRequest -> Put
writeToBuffer icmp = do
    let
        (Type pt) = packetType icmp
        (Code co) = code icmp
        (CheckSum ch) = checkSum icmp
        (Indentifier i) = id_ icmp
        (Sequence s) = sequence__ icmp
        (ICMPData d) = timeStamp icmp
    putWord8 $ pt
    putWord8 $ co
    putWord16be $ ch
    putWord16be $ i
    putWord16be $ s
    putWord64be $ d
  
buildRequest :: Indentifier -> Sequence -> ICMPData -> ICMPRequest
buildRequest identifier sequence__ timeStamp = ICMPRequest (Type 8) (Code 0) (CheckSum checkSum) identifier sequence__ timeStamp
    where
        --build an empty packet so we can calc a check sum
        initialPacket = ICMPRequest (Type 8) (Code 0) (CheckSum 0) identifier sequence__ timeStamp
        
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
        