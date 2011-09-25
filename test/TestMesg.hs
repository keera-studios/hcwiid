module Main where

import Prelude
import Control.Monad
import System.CWiid

main :: IO ()
main = do
  putStrLn "Put Wiimote in discoverable mode now (press 1+2)..."
  (Just wm) <- cwiidOpen
  putStrLn "found!"
  _ <- cwiidSetLed wm
  _ <- cwiidSetRptMode wm
  _ <- cwiidEnable wm
  _ <- forever $ cwiidGetMesg wm >>= print
  return () -- not reach
