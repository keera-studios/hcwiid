{-# LANGUAGE ForeignFunctionInterface #-}

module System.CWiid
       (cwiidOpen, cwiidSetLed, cwiidSetRptMode, cwiidGetBtnState,
        cwiidLed1, cwiidLed2, cwiidLed3, cwiidLed4, combineCwiidLedFlag,
        cwiidBtn2, cwiidBtn1, cwiidBtnB, cwiidBtnA, cwiidBtnMinus,
        cwiidBtnHome, cwiidBtnLeft, cwiidBtnRight, cwiidBtnDown, cwiidBtnUp,
        cwiidBtnPlus, combineCwiidBtnFlag, diffCwiidBtnFlag,
        cwiidEnable, cwiidGetMesg,
        CWiidBtnFlag(..), CWiidState(..), CWiidWiimote) where

-- import Foreign.C.Error
import Data.Bits
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

#include <cwiid.h>

-----------------------------------------------------------------------------
-- Data type
---

-- typedef struct {
--         uint8_t b[6];
-- } __attribute__((packed)) bdaddr_t;
-- #define BDADDR_ANY   (&(bdaddr_t) {{0, 0, 0, 0, 0, 0}})
data CWiidBdaddr = CWiidBdaddr CInt CInt CInt CInt CInt CInt
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
newtype CWiidWiimote = CWiidWiimote { unCWiidWiimote :: Ptr () }

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
newtype CWiidLedFlag = CWiidLedFlag { unCWiidLedFlag :: CInt }
                     deriving (Eq, Show)
#{enum CWiidLedFlag, CWiidLedFlag
 , cwiidLed1 = CWIID_LED1_ON
 , cwiidLed2 = CWIID_LED2_ON
 , cwiidLed3 = CWIID_LED3_ON
 , cwiidLed4 = CWIID_LED4_ON
 }
combineCwiidLedFlag :: [CWiidLedFlag] -> CWiidLedFlag
combineCwiidLedFlag = CWiidLedFlag . foldr ((.|.) . unCWiidLedFlag) 0

newtype CWiidEnableFlag = CWiidEnableFlag { unCWiidEnableFlag :: CInt }
                     deriving (Eq, Show)
#{enum CWiidEnableFlag, CWiidEnableFlag
 , cwiidFlagMesgIfc = CWIID_FLAG_MESG_IFC
 , cwiidFlagContinuous = CWIID_FLAG_CONTINUOUS
 , cwiidFlagRepartBtn = CWIID_FLAG_REPEAT_BTN
 , cwiidFlagNonblock = CWIID_FLAG_NONBLOCK
 , cwiidFlagMotionplus = CWIID_FLAG_MOTIONPLUS
 }
combineCwiidEnableFlag :: [CWiidEnableFlag] -> CWiidEnableFlag
combineCwiidEnableFlag = CWiidEnableFlag . foldr ((.|.) . unCWiidEnableFlag) 0

newtype CWiidBtnFlag = CWiidBtnFlag { unCWiidBtnFlag :: CInt }
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

data CWiidState = CWiidState { rptMode :: CInt, led :: CInt, rumble :: CInt, 
                               battery :: CInt, buttons :: CInt } -- xxx 定義不足
                deriving Show
instance Storable CWiidState where
  sizeOf = const #size struct cwiid_state
  alignment = sizeOf
  poke cwst (CWiidState rp l ru ba bu) = do
    (#poke struct cwiid_state, rpt_mode) cwst rp
    (#poke struct cwiid_state, led) cwst l
    (#poke struct cwiid_state, rumble) cwst ru
    (#poke struct cwiid_state, battery) cwst ba
    (#poke struct cwiid_state, buttons) cwst bu
  peek cwst = do
    rp <- (#peek struct cwiid_state, rpt_mode) cwst
    l <- (#peek struct cwiid_state, led) cwst
    ru <- (#peek struct cwiid_state, rumble) cwst
    ba <- (#peek struct cwiid_state, battery) cwst
    bu <- (#peek struct cwiid_state, buttons) cwst
    return $ CWiidState rp l ru ba bu

{--
union cwiid_mesg {
        enum cwiid_mesg_type type;
        struct cwiid_status_mesg status_mesg;
        struct cwiid_btn_mesg btn_mesg;
        struct cwiid_acc_mesg acc_mesg;
        struct cwiid_ir_mesg ir_mesg;
        struct cwiid_nunchuk_mesg nunchuk_mesg;
        struct cwiid_classic_mesg classic_mesg;
        struct cwiid_balance_mesg balance_mesg;
        struct cwiid_motionplus_mesg motionplus_mesg;
        struct cwiid_error_mesg error_mesg;
};
--}
data CWiidMesg = CWMStat | CWMBtn CInt | CWMAcc | CWMIr | CWMnunchuk | 
                 CWMClassic | CWMBalance | CWMMotionPlus | CWMError
               deriving Show -- xxx 定義不足
instance Storable CWiidMesg where
  sizeOf = const #size union cwiid_mesg
  alignment = sizeOf
  poke _ _ = error "hoge!" -- xxx 書けないよ!
  peek mesg = do
    mType <- (#peek union cwiid_mesg, type) mesg
    case mType::CInt of
      (#const CWIID_MESG_BTN) -> do
        b <- (#peek union cwiid_mesg, btn_mesg.buttons) mesg
        return $ CWMBtn b
      otherwise -> return CWMError -- xxx

-----------------------------------------------------------------------------
-- Haskell land
---
-- wiimote = cwiid_open(&bdaddr, 0)))
cwiidOpen :: IO (Maybe CWiidWiimote)
cwiidOpen =
  alloca $ \bdAddr -> do
    poke bdAddr $ CWiidBdaddr 0 0 0 0 0 0
    handle <- c_cwiid_open bdAddr 0 -- エラー処理必要
    if handle == nullPtr
      then return Nothing
      else return $ Just $ CWiidWiimote handle

cwiidSetLed :: CWiidWiimote -> IO CInt
cwiidSetLed wm = c_cwiid_set_led  handle 9 -- set on LED 1 and 4
  where handle = unCWiidWiimote wm

cwiidSetRptMode :: CWiidWiimote -> IO CInt
cwiidSetRptMode wm = c_cwiid_set_rpt_mode handle 6 -- set BTN and ACC
  where handle = unCWiidWiimote wm

cwiidGetBtnState :: CWiidWiimote -> IO CWiidBtnFlag
cwiidGetBtnState wm =
  alloca $ \wiState -> do
    _ <- c_cwiid_get_state handle wiState
    ws <- peek wiState
    return $ CWiidBtnFlag $ buttons ws
      where handle = unCWiidWiimote wm

cwiidEnable :: CWiidWiimote -> IO CInt
cwiidEnable wm = c_cwiid_enable handle (unCWiidEnableFlag cwiidFlagMesgIfc)
  where handle = unCWiidWiimote wm

-- xxx たぶんメモリリークしてる
cwiidGetMesg :: CWiidWiimote -> IO [CWiidMesg]
cwiidGetMesg wm =
  alloca (\pNum ->
           alloca (\ppMesg -> do
                      let handle = unCWiidWiimote wm
                      ret <- c_cwiid_get_mesg handle pNum ppMesg
                      if ret /= 0
                        then return []
                        else do num <- peek pNum
                                pMesg <- peek ppMesg
                                mesgs <- peekArray (fromIntegral num) pMesg
                                return mesgs))

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

-- int cwiid_get_state(cwiid_wiimote_t *wiimote, struct cwiid_state *state);
foreign import ccall safe "cwiid_get_state" c_cwiid_get_state
  :: Ptr () -> Ptr CWiidState -> IO CInt

-- int cwiid_enable(cwiid_wiimote_t *wiimote, int flags);
foreign import ccall safe "cwiid_enable" c_cwiid_enable
  :: Ptr () -> CInt -> IO CInt

-- int cwiid_disable(cwiid_wiimote_t *wiimote, int flags);
foreign import ccall safe "cwiid_disable" c_cwiid_disable
  :: Ptr () -> CInt -> IO CInt

-- int cwiid_get_mesg(cwiid_wiimote_t *wiimote, int *mesg_count, union cwiid_mesg *mesg[]);
foreign import ccall safe "cwiid_get_mesg" c_cwiid_get_mesg
  :: Ptr () -> Ptr CInt -> Ptr (Ptr CWiidMesg) -> IO CInt

-- C => Haskell
---

-- int cwiid_set_mesg_callback(cwiid_wiimote_t *wiimote,
--                             cwiid_mesg_callback_t *callback)
-- xxxxx
-- typedef void cwiid_mesg_callback_t(cwiid_wiimote_t *, int,
--                                    union cwiid_mesg [], struct timespec *)
-- xxxxx
