module SampleNetworking
    ( getGoogleHomepage,
    ) where

import Network
import System.IO

getGoogleHomepage :: IO ()
getGoogleHomepage = withSocketsDo $ do
                      h <- connectTo "www.crossoverdance.com" (PortNumber 80)
                      hSetBuffering h LineBuffering
                      hPutStr h "Get / HTTP/1.1\nhost: www.google.com\n\n"
                      contents <- hGetContents h
                      putStrLn contents
                      hClose h
