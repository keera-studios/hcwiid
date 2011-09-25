module Main where

import Control.Monad
import System.CWiid
import System.Posix.Unistd

main :: IO ()
main = do
  putStrLn "Put Wiimote in discoverable mode now (press 1+2)..."
  (Just wm) <- cwiidOpen cwiidBdaddrAny
  putStrLn "found!"
  _ <- cwiidSetLed wm [cwiidLed1, cwiidLed4]
  _ <- cwiidSetRptMode wm [cwiidRptBtn]
  _ <- forever $ do _ <- usleep 300000
                    cwiidGetBtnState wm >>= print
  return () -- not reach
