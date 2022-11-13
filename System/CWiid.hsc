{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      :  System.CWiid
-- Copyright   :  Kiwamu Okabe, Ivan Perez and the cwiid team
-- License     :  GPL-2
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  experimental
-- Portability :  unknown
--
-- Bindings for the cwiid library, a working userspace driver
-- along with various applications implementing event drivers,
-- multiple Wiimote connectivity, gesture recognition, and other
-- Wiimote-based functionality.
--
-- The current implementation is rather incomplete. In particular:
--
-- * Some Haskell functions (those related to rpt mode, rumble, leds)
-- had hard-coded values in them. Therefore, they implemented only a
-- very partial interface to their C counterparts. The new versions
-- should be tested and, if any other function is like this,
-- then exported properly.
--
-- * Not all functions/wiimote fields are accessible. In particular,
-- acceleromoter and IR is in testing stage. Nunchuck, calibration,
-- wiimote plus are not handled at all (but will be in the future).
--
-- All in all, the code works quite well and is currently being used
-- to implement several real games.

module System.CWiid
    (
      -- * Initialization
      cwiidOpen
    , CWiidWiimote
      -- * State
    , CWiidState(..)
      -- * Reception mode
    , cwiidSetRptMode
      -- * Leds
    , CWiidLedFlag
    , cwiidLed1
    , cwiidLed2
    , cwiidLed3
    , cwiidLed4
      -- ** Led operations
    , cwiidSetLed
    , combineCwiidLedFlag
      -- * Rumble
    , cwiidSetRumble
      -- * Buttons
    , cwiidGetBtnState
    , cwiidIsBtnPushed
    , cwiidBtn1
    , cwiidBtn2
    , cwiidBtnA
    , cwiidBtnB
    , cwiidBtnPlus
    , cwiidBtnMinus
    , cwiidBtnHome
    , cwiidBtnLeft
    , cwiidBtnRight
    , cwiidBtnDown
    , cwiidBtnUp
    , combineCwiidBtnFlag
    , diffCwiidBtnFlag
    , CWiidBtnFlag(..)
      -- * Accelerometers
    , cwiidGetAcc
    , CWiidAcc(..)
      -- * Infra-red
    , CWiidIRSrc(..)
    , cwiidGetIR
    )
  where

-- External imports
import Data.Bits        ((.&.), (.|.))
import Foreign.C.Types  (CChar (..), CInt (..), CUChar (..), CUShort (..))
import Foreign.Marshal  (alloca, peekArray, pokeArray)
import Foreign.Ptr      (Ptr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))

#include <cwiid.h>

-----------------------------------------------------------------------------
-- Data type
---

-- typedef struct {
--         uint8_t b[6];
-- } __attribute__((packed)) bdaddr_t;
-- #define BDADDR_ANY   (&(bdaddr_t) {{0, 0, 0, 0, 0, 0}})
data CWiidBdaddr = CWiidBdaddr Int Int Int Int Int Int
instance Storable CWiidBdaddr where
  sizeOf = const #size bdaddr_t
  alignment = sizeOf
  poke bdat (CWiidBdaddr b0 b1 b2 b3 b4 b5) = do
    (#poke bdaddr_t, b[0]) bdat b0
    (#poke bdaddr_t, b[1]) bdat b1
    (#poke bdaddr_t, b[2]) bdat b2
    (#poke bdaddr_t, b[3]) bdat b3
    (#poke bdaddr_t, b[4]) bdat b4
    (#poke bdaddr_t, b[5]) bdat b5
  peek bdat = do
    b0 <- (#peek bdaddr_t, b[0]) bdat
    b1 <- (#peek bdaddr_t, b[1]) bdat
    b2 <- (#peek bdaddr_t, b[2]) bdat
    b3 <- (#peek bdaddr_t, b[3]) bdat
    b4 <- (#peek bdaddr_t, b[4]) bdat
    b5 <- (#peek bdaddr_t, b[5]) bdat
    return $ CWiidBdaddr b0 b1 b2 b3 b4 b5

-- typedef struct wiimote cwiid_wiimote_t;
--
-- | A connection to an existing wiimote. Use 'cwiidOpen' to
-- connect to a wiimote and obtain one of these.
newtype CWiidWiimote = CWiidWiimote { unCWiidWiimote :: Ptr () }

-- | Try to establish a connection to any existing Wiimote using
-- any existing bluetooth interface.
--
-- The function returns 'Nothing' if there is no bluetooth interface
-- or if no wiimote can be located. If the connection succeeds,
-- a 'CWiidWiimote' is returned (inside a 'Just'), which can be used to
-- poll the wiimote using other functions.
--
-- There is a default timeout of 5 seconds.
--
-- * TODO: export cwiid_open_time and cwiid_close as well.

-- wiimote = cwiid_open(&bdaddr, 0)))
cwiidOpen :: IO (Maybe CWiidWiimote)
cwiidOpen =
  alloca $ \bdAddr -> do
    poke bdAddr $ CWiidBdaddr 0 0 0 0 0 0
    handle <- c_cwiid_open bdAddr 0 -- エラー処理必要
    if handle == nullPtr
      then return Nothing
      else return $ Just $ CWiidWiimote handle

{--
struct cwiid_state {
        uint8_t rpt_mode;
        uint8_t led;
        uint8_t rumble;
        uint8_t battery;
        uint16_t buttons;
        uint8_t acc[3];
        struct cwiid_ir_src ir_src[CWIID_IR_SRC_COUNT];
        enum cwiid_ext_type ext_type;
        union ext_state ext;
        enum cwiid_error error;
};
--}

-- | The state of the wiimote. Use 'cwiidSetRptMode' to enable/disable
-- sensors.
--
-- * FIXME: incomplete state
-- * FIXME: export get_state
data CWiidState = CWiidState
  { rptMode :: Int, led :: Int, rumble :: Int,
    battery :: Int, buttons :: Int, acc :: [Int]
  , irSrc   :: [CWiidIRSrc]
  }
  deriving Show

instance Storable CWiidState where
  sizeOf = const #size struct cwiid_state
  alignment = sizeOf
  poke cwst (CWiidState rp l ru ba bu [ac0,ac1,ac2] irs) = do
    (#poke struct cwiid_state, rpt_mode) cwst rp
    (#poke struct cwiid_state, led) cwst l
    (#poke struct cwiid_state, rumble) cwst ru
    (#poke struct cwiid_state, battery) cwst ba
    (#poke struct cwiid_state, buttons) cwst bu
    (#poke struct cwiid_state, acc[0]) cwst (fromIntegral ac0 :: CUChar)
    (#poke struct cwiid_state, acc[1]) cwst (fromIntegral ac1 :: CUChar)
    (#poke struct cwiid_state, acc[2]) cwst (fromIntegral ac2 :: CUChar)
    pokeArray ((#ptr struct cwiid_state, ir_src) cwst) irs
  peek cwst = do
    rp <- (#peek struct cwiid_state, rpt_mode) cwst
    l <- (#peek struct cwiid_state, led) cwst
    ru <- (#peek struct cwiid_state, rumble) cwst
    ba <- (#peek struct cwiid_state, battery) cwst
    bu <- (#peek struct cwiid_state, buttons) cwst
    ac0 <- (#peek struct cwiid_state, acc[0]) cwst
    ac1 <- (#peek struct cwiid_state, acc[1]) cwst
    ac2 <- (#peek struct cwiid_state, acc[2]) cwst
    irs <- peekArray cwiidIrSrcCount ((#ptr struct cwiid_state, ir_src) cwst)
    return $ CWiidState rp l ru ba bu [ fromIntegral (ac0 :: CUChar)
                                      , fromIntegral (ac1 :: CUChar)
                                      , fromIntegral (ac2 :: CUChar)]
                                      irs

-- * Infrared

-- | Maximum number of infrared points detected.
--   By default (according to cwiid) it should be 4.
cwiidIrSrcCount :: Int
cwiidIrSrcCount = (#const CWIID_IR_SRC_COUNT)

-- struct cwiid_ir_src {
-- 	char valid;
-- 	uint16_t pos[2];
-- 	int8_t size;
-- };
--
-- The following model is weaker than the counterpart in C (see above). We do
-- so in order to provide something more "natural" in Haskell, but it might
-- be better to use a more precise datatype.

-- | Internal representation of an infrared point. You should no use it
--   unless you know what you are doing; use 'CWiidIR' instead.
data CWiidIRSrc = CWiidIRSrc
  { cwiidIRSrcValid :: Bool
  , cwiidIRSrcPosX  :: Int
  , cwiidIRSrcPosY  :: Int
  , cwiidIRSrcSize  :: Int
  }
 deriving Show

instance Storable CWiidIRSrc where
  sizeOf = const #size struct cwiid_ir_src
  alignment = sizeOf
  poke cwst (CWiidIRSrc valid posX posY sz) = do
    (#poke struct cwiid_ir_src, valid)  cwst ((if valid then (-1) else 0) :: CChar)
    (#poke struct cwiid_ir_src, pos[0]) cwst (fromIntegral posX :: CUShort)
    (#poke struct cwiid_ir_src, pos[1]) cwst (fromIntegral posY :: CUShort)
    (#poke struct cwiid_ir_src, size)   cwst (fromIntegral sz   :: CChar)
  peek cwst = do
    valid <- (#peek struct cwiid_ir_src, valid)  cwst
    posX  <- (#peek struct cwiid_ir_src, pos[0]) cwst
    posY  <- (#peek struct cwiid_ir_src, pos[1]) cwst
    sz    <- (#peek struct cwiid_ir_src, size)   cwst
    return $ CWiidIRSrc (not ((valid :: CChar) == 0))
                        (fromIntegral (posX :: CUShort))
                        (fromIntegral (posY :: CUShort))
                        (fromIntegral (sz :: CChar))

cwiidGetIR :: CWiidWiimote -> IO [CWiidIRSrc]
cwiidGetIR wm =
  alloca $ \wiState -> do
    _ <- c_cwiid_get_state handle wiState
    ws <- peek wiState
    return (irSrc ws)
      where handle = unCWiidWiimote wm

-- * Leds
newtype CWiidLedFlag = CWiidLedFlag { unCWiidLedFlag :: Int }
                     deriving (Eq, Show)

-- | Flag with exactly led 1 enabled. Use 'combineCwiidLedFlag'
--   to create flags with several leds enabled.
#{enum CWiidLedFlag, CWiidLedFlag
 , cwiidLed1 = CWIID_LED1_ON
 }

-- | Flag with exactly led 2 enabled. Use 'combineCwiidLedFlag'
--   to create flags with several leds enabled.
#{enum CWiidLedFlag, CWiidLedFlag
 , cwiidLed2 = CWIID_LED2_ON
 }

-- | Flag with exactly led 2 enabled. Use 'combineCwiidLedFlag'
--   to create flags with several leds enabled.
#{enum CWiidLedFlag, CWiidLedFlag
 , cwiidLed3 = CWIID_LED3_ON
 }

-- | Flag with exactly led 4 enabled. Use 'combineCwiidLedFlag'
--   to create flags with several leds enabled.
#{enum CWiidLedFlag, CWiidLedFlag
 , cwiidLed4 = CWIID_LED4_ON
 }

-- | Enable/disable certain leds.
--
--   Use 'cwiidLed1' .. 'cwiidLed4' together with 'combineCwiidLedFlag'
--   to create a flag with just the leds you want enabled and change
--   all at once with one operation.
cwiidSetLed :: CWiidWiimote -> CWiidLedFlag -> IO CInt
cwiidSetLed wm leds = c_cwiid_set_led handle ledUChars
  where handle    = unCWiidWiimote wm
        ledUChars = fromIntegral (unCWiidLedFlag leds)

-- | Combine several led flags into one led flag with those leds
--   enabled and all other leds disabled.

combineCwiidLedFlag :: [CWiidLedFlag] -> CWiidLedFlag
combineCwiidLedFlag = CWiidLedFlag . foldr ((.|.) . unCWiidLedFlag) 0

-- * Buttons

newtype CWiidBtnFlag = CWiidBtnFlag { unCWiidBtnFlag :: Int }
                     deriving (Eq, Show)
#{enum CWiidBtnFlag, CWiidBtnFlag
 , cwiidBtn2     = CWIID_BTN_2
 , cwiidBtn1     = CWIID_BTN_1
 , cwiidBtnB     = CWIID_BTN_B
 , cwiidBtnA     = CWIID_BTN_A
 , cwiidBtnMinus = CWIID_BTN_MINUS
 , cwiidBtnHome  = CWIID_BTN_HOME
 , cwiidBtnLeft  = CWIID_BTN_LEFT
 , cwiidBtnRight = CWIID_BTN_RIGHT
 , cwiidBtnDown  = CWIID_BTN_DOWN
 , cwiidBtnUp    = CWIID_BTN_UP
 , cwiidBtnPlus  = CWIID_BTN_PLUS
 }

combineCwiidBtnFlag :: [CWiidBtnFlag] -> CWiidBtnFlag
combineCwiidBtnFlag = CWiidBtnFlag . foldr ((.|.) . unCWiidBtnFlag) 0

diffCwiidBtnFlag :: CWiidBtnFlag -> CWiidBtnFlag -> CWiidBtnFlag
diffCwiidBtnFlag a b = CWiidBtnFlag $ ai - (ai .&. bi)
  where ai = unCWiidBtnFlag a
        bi = unCWiidBtnFlag b

-- * Reception mode

-- | Reception modes that select which sensors/wiimote activity
-- we listen to.
newtype CWiidRptMode = CWiidRptMode { unCWiidRptMode :: CUChar }
  deriving (Eq, Show)

-- | Enable/disable reception of certain sensors.
-- Use 2 to enable buttons.
cwiidSetRptMode :: CWiidWiimote -> CUChar -> IO CInt
cwiidSetRptMode wm u = c_cwiid_set_rpt_mode handle u -- set BTN
  where handle = unCWiidWiimote wm

-- * Rumble

cwiidSetRumble :: CWiidWiimote -> CUChar -> IO CInt
cwiidSetRumble wm rm = c_cwiid_set_rumble handle rm
  where handle = unCWiidWiimote wm

-- * Buttons

-- | Returns a mask with the buttons that are currently pushed.
cwiidGetBtnState :: CWiidWiimote -> IO CWiidBtnFlag
cwiidGetBtnState wm =
  alloca $ \wiState -> do
    _ <- c_cwiid_get_state handle wiState
    ws <- peek wiState
    return $ CWiidBtnFlag $ buttons ws
      where handle = unCWiidWiimote wm

-- | Returns 'True' if the button indicated by the flag is pushed,
-- 'False' otherwise.
--
-- This is a pure function, so the first argument must be the
-- button flags as returned by 'cwiidGetBtnState'.
cwiidIsBtnPushed :: CWiidBtnFlag -- ^ The button flags as returned by 'cwiidGetBtnState'.
                 -> CWiidBtnFlag -- ^ A mask that flags the button/s that we want to check.
                 -> Bool         -- ^ 'True' if they are all pushed, 'False' otherwise.
cwiidIsBtnPushed flags btn =
  unCWiidBtnFlag flags .&. unCWiidBtnFlag btn == unCWiidBtnFlag btn

-- * Accelerometres

-- | Array of accelerometer information. It will always contain
-- exactly three elements.
--
-- * TODO: provide a more informative and restrictive interface
-- with exactly three named Int (byte?) fields.
--
newtype CWiidAcc = CWiidAcc { unCWiidAcc :: [Int] }
 deriving (Eq, Show)

-- | Obtain accelerometer information.
--   FIXME: read wmgui/main.c:cwiid_acc(1119) to understand how to use
--   this information, what else might need to be exported, and how
--   to calibrate the accelerometers.
cwiidGetAcc :: CWiidWiimote -> IO CWiidAcc
cwiidGetAcc wm =
  alloca $ \wiState -> do
    _ <- c_cwiid_get_state handle wiState
    ws <- peek wiState
    return $ CWiidAcc $ acc ws
      where handle = unCWiidWiimote wm

-- * Low-level bindings to C functions and back

-----------------------------------------------------------------------------
-- C land
---
-- Haskell => C
---

-- cwiid_wiimote_t *cwiid_open(bdaddr_t *bdaddr, int flags)
foreign import ccall safe "cwiid_open" c_cwiid_open
  :: Ptr CWiidBdaddr -> CInt -> IO (Ptr ())

-- typedef unsigned char             uint8_t
-- int cwiid_set_led(cwiid_wiimote_t *wiimote, uint8_t led)
foreign import ccall safe "cwiid_set_led" c_cwiid_set_led
  :: Ptr () -> CUChar -> IO CInt

-- int cwiid_set_rpt_mode(cwiid_wiimote_t *wiimote, uint8_t rpt_mode);
foreign import ccall safe "cwiid_set_rpt_mode" c_cwiid_set_rpt_mode
  :: Ptr () -> CUChar -> IO CInt

-- int cwiid_set_rumble(cwiid_wiimote_t *wiimote, uint8_t rumble);
foreign import ccall safe "cwiid_set_rumble" c_cwiid_set_rumble
  :: Ptr () -> CUChar -> IO CInt

-- int cwiid_get_state(cwiid_wiimote_t *wiimote, struct cwiid_state *state);
foreign import ccall safe "cwiid_get_state" c_cwiid_get_state
  :: Ptr () -> Ptr CWiidState -> IO CInt


-- C => Haskell
---

-- int cwiid_set_mesg_callback(cwiid_wiimote_t *wiimote,
--                             cwiid_mesg_callback_t *callback)
-- xxxxx
-- typedef void cwiid_mesg_callback_t(cwiid_wiimote_t *, int,
--                                    union cwiid_mesg [], struct timespec *)
-- xxxxx
