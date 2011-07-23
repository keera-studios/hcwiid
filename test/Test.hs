module Main where

import Prelude
import System.CWiid
import System.Posix.Unistd

main :: IO ()
main = do
  print "finding wiimote..."
  wm <- cwiidOpen
  print "found!"
  cwiidSetLed wm
  sleep 10
  return ()
