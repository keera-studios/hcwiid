{-# LANGUAGE ForeignFunctionInterface #-}

module System.CWiid (cwiidOpen, cwiidSetLed) where

-- import Foreign.C.Error
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
newtype CWiidWiimote = CWiidWiimote (Ptr CWiidWiimote)

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



-----------------------------------------------------------------------------
-- Haskell land
---
-- wiimote = cwiid_open(&bdaddr, 0)))
cwiidOpen :: IO CWiidWiimote
cwiidOpen =
  alloca $ \bdAddr -> do
    poke bdAddr $ CWiidBdaddr 0 0 0 0 0 0
    c_cwiid_open bdAddr 0 -- エラー処理必要

cwiidSetLed :: CWiidWiimote -> IO CInt
cwiidSetLed wm = c_cwiid_set_led wm 1

-----------------------------------------------------------------------------
-- C land
---
-- Haskell => C
---

-- cwiid_wiimote_t *cwiid_open(bdaddr_t *bdaddr, int flags)
foreign import ccall safe "cwiid_open" c_cwiid_open
  :: Ptr a -> CInt -> IO CWiidWiimote
--  :: Ptr (#type bdaddr_t) -> CInt -> IO (Ptr (#type cwiid_wiimote_t))

-- typedef unsigned char             uint8_t
-- int cwiid_set_led(cwiid_wiimote_t *wiimote, uint8_t led)
foreign import ccall safe "cwiid_set_led" c_cwiid_set_led
  :: CWiidWiimote -> CUChar -> IO CInt
--  :: Ptr (#type cwiid_wiimote_t) -> CUChar -> IO (CInt)


-- C => Haskell
---

-- int cwiid_set_mesg_callback(cwiid_wiimote_t *wiimote,
--                             cwiid_mesg_callback_t *callback)
-- xxxxx

-- typedef void cwiid_mesg_callback_t(cwiid_wiimote_t *, int,
--                                    union cwiid_mesg [], struct timespec *)
-- xxxxx
