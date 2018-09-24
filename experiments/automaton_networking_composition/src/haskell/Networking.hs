module Networking
    ( createSocket,
      getNPackets,
    ) where

import qualified Network.Socket as S hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Numeric as N

import Foreign.C
foreign import ccall unsafe "htons" c_htons :: CInt -> CInt

{- Creates an ethernet socket which returns IP frames.
 - Will return ALL IP PACKETS received on the interface - promiscuous mode is
 - just a side effect of having an AF_PACKET socket.
 -}
createSocket :: IO S.Socket
createSocket = do
    -- #define ETH_P_IP 0x0800
    sock <- S.socket S.AF_PACKET S.Datagram $ c_htons 0x0800
    S.setSocketOption sock S.ReuseAddr 1
    return sock

getNPackets :: (Integral a) => a -> S.Socket -> IO ()
getNPackets 1 s = getPacket s
getNPackets n s
    | n <= 0 = putStrLn "why are you like this...."
    | otherwise = do
        getPacket s
        getNPackets (n-1) s

getPacket :: S.Socket -> IO ()
getPacket s = do
    pkt <- BS.recv s 2048
    let hex_pkt = foldr ($) "" $ map N.showHex (map fromEnum $ BC.unpack pkt)
    putStrLn hex_pkt
    packetType $ head hex_pkt

packetType :: Char -> IO ()
packetType '4' = putStrLn "Got an IPv4 packet."
packetType '6' = putStrLn "Got an IPv6 packet."
packetType _ = putStrLn "Idk what packet I got..."
