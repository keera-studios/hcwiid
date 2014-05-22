module Main where

import Prelude
import Control.Monad
import System.CWiid
import System.Posix.Unistd
-- Uncomment if you want to move the mouse with the wiimote
-- import System.Process

main :: IO ()
main = do
  putStrLn "Put Wiimote in discoverable mode now (press 1+2)..."
  (Just wm) <- cwiidOpen
  putStrLn "found!"
  _ <- cwiidSetLed wm (combineCwiidLedFlag [cwiidLed1, cwiidLed3])
  _ <- cwiidSetRptMode wm 15 -- Buttons + Acc + IR
  _ <- forever $ do _ <- usleep 300000
                    cwiidGetBtnState wm >>= print
                    cwiidGetAcc wm >>= print
                    cwiidGetIR wm >>= print

                    -- The following code shows how to use the IR camera
                    -- to control the mouse. Adjust your monitor resolution
                    -- and displacement using the constants below.
                    --
                    -- irs <- cwiidGetIR wm
                    -- let numLeds = filter cwiidIRSrcValid rs
                    -- let led1   = irs!!0
                    --     led2   = irs!!1
                    -- let posX = ((cwiidIRSrcPosX led1) + (cwiidIRSrcPosX led2)) `div` 2
                    -- let posY = ((cwiidIRSrcPosY led1) + (cwiidIRSrcPosY led2)) `div` 2
                    --     propX = (fromIntegral (1024 - posX)) / 1024.0
                    --     propY = (fromIntegral (max 0 (posY - 384))) / 384.0 
                    --     finX  = monitorDispX + round (monitorResX * propX)
                    --     finY  = monitorDispY + round (monitorResY * propY)

                    -- system $ "xdotool mousemove " ++ show finX ++ " " ++ show finY
                    
  return () -- not reach

-- monitorResX = 1280
-- monitorResY = 1024
-- 
-- monitorDispX = 1920
-- monitorDispY = 0
