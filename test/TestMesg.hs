module Main where

import Prelude
import Control.Monad
import System.CWiid

main :: IO ()
main = do
  putStrLn "Put Wiimote in discoverable mode now (press 1+2)..."
  (Just wm) <- cwiidOpen cwiidBdaddrAny
  putStrLn "found!"
  _ <- cwiidSetLed wm [cwiidLed2, cwiidLed4]
  _ <- cwiidSetRptMode wm [cwiidRptBtn]
  _ <- cwiidEnable wm [cwiidFlagMesgIfc]
  _ <- forever $ cwiidGetMesg wm >>= print
  return () -- not reach
