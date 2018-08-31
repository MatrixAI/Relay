module SampleNetworking
    ( createSocket,
      getPacket,
    ) where

import qualified Network.Socket as S hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Numeric as N
import Foreign.C

foreign import ccall unsafe "htons" c_htons :: CInt -> CInt

createSocket :: IO S.Socket
createSocket = do
        -- #define ETH_P_IP 0x0800
        sock <- S.socket S.AF_PACKET S.Datagram $ c_htons 0x0800
        S.setSocketOption sock S.ReuseAddr 1
        return sock

getPacket :: S.Socket -> IO ()
getPacket s = do
        pkt <- BS.recv s 2048
        packetType $ head $ foldr ($) "" $ map N.showHex (map fromEnum $ BC.unpack pkt)

packetType :: Char -> IO ()
packetType '4' = putStrLn "Got an IPv4 packet."
packetType '6' = putStrLn "Got an IPv6 packet."
packetType _ = putStrLn "Idk what packet I got..."
