module Files.Binary where

import Graph.Types
import Ping.Types

instance (Binary a, Binary b) => Binary (Graph a b) where
