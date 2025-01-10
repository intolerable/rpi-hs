module System.IO.LibGPIOD
  ( module System.IO.LibGPIOD
  ) where

import Data.Bits
import Foreign.C.ConstPtr
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Clock

#include "gpiod.h"

-- * GPIO chip operations

data GPIODChip

foreign import capi "gpiod.h"
  gpiod_chip_open :: ConstPtr CChar -> IO (Ptr GPIODChip)

foreign import capi "gpiod.h"
  gpiod_chip_open_by_name :: ConstPtr CChar -> IO (Ptr GPIODChip)

foreign import capi "gpiod.h"
  gpiod_chip_open_by_number :: Word -> IO (Ptr GPIODChip)

foreign import capi "gpiod.h"
  gpiod_chip_open_by_label :: ConstPtr CChar -> IO (Ptr GPIODChip)

foreign import capi "gpiod.h"
  gpiod_chip_open_lookup :: ConstPtr CChar -> IO (Ptr GPIODChip)

foreign import capi "gpiod.h"
  gpiod_chip_close :: Ptr GPIODChip -> IO ()

foreign import capi "gpiod.h &gpiod_chip_close"
  gpiod_chip_close_p :: FunPtr (Ptr GPIODChip -> IO ())

foreign import capi "gpiod.h"
  gpiod_chip_name :: Ptr GPIODChip -> IO (ConstPtr CChar)

foreign import capi "gpiod.h"
  gpiod_chip_label :: Ptr GPIODChip -> IO (ConstPtr CChar)

foreign import capi "gpiod.h"
  gpiod_chip_num_lines :: Ptr GPIODChip -> IO CInt


-- * GPIO line operations

data GPIODLine

foreign import capi "gpiod.h"
  gpiod_chip_get_line :: Ptr GPIODChip -> CUInt -> IO (Ptr GPIODLine)

foreign import capi "gpiod.h"
  gpiod_chip_get_lines :: Ptr GPIODChip -> Ptr CUInt -> CUInt -> Ptr GPIODLineBulk -> IO CInt

foreign import capi "gpiod.h"
  gpiod_chip_get_all_lines :: Ptr GPIODChip -> Ptr GPIODLineBulk -> IO CInt

foreign import capi "gpiod.h"
  gpiod_chip_find_line :: Ptr GPIODChip -> ConstPtr CChar -> IO (Ptr GPIODLine)

foreign import capi "gpiod.h"
  gpiod_chip_find_lines :: Ptr GPIODChip -> ConstPtr (ConstPtr CChar) -> Ptr GPIODLineBulk -> IO CInt


-- * GPIO bulk line operations

gpiodLineBulkMaxLines :: CUInt
gpiodLineBulkMaxLines = #{const GPIOD_LINE_BULK_MAX_LINES}

data GPIODLineBulk = GPIODLineBulk
  { lines :: Ptr GPIODLine
  , numLines :: CUInt
  } deriving (Show, Eq, Ord)

instance Storable GPIODLineBulk where
  sizeOf _ = #{size struct gpiod_line_bulk}
  alignment _ = #{alignment struct gpiod_line_bulk}
  peek p =
    GPIODLineBulk <$> #{peek struct gpiod_line_bulk, lines} p
                  <*> #{peek struct gpiod_line_bulk, num_lines} p
  poke p (GPIODLineBulk l n) = do
    #{poke struct gpiod_line_bulk, lines} p l
    #{poke struct gpiod_line_bulk, num_lines} p n


-- * Line info

newtype GPIODLineDirection = GPIODLineDirection CInt
  deriving (Show, Eq, Ord, Storable)

pattern GPIODLineDirectionInput :: GPIODLineDirection
pattern GPIODLineDirectionInput =
  GPIODLineDirection #{const GPIOD_LINE_DIRECTION_INPUT}

pattern GPIODLineDirectionOutput :: GPIODLineDirection
pattern GPIODLineDirectionOutput =
  GPIODLineDirection #{const GPIOD_LINE_DIRECTION_OUTPUT}

{-# COMPLETE GPIODLineDirectionInput, GPIODLineDirectionOutput #-}

newtype GPIODLineActiveState = GPIODLineActiveState CInt
  deriving (Show, Eq, Ord, Storable)

pattern GPIODLineActiveStateHigh :: GPIODLineActiveState
pattern GPIODLineActiveStateHigh =
  GPIODLineActiveState #{const GPIOD_LINE_ACTIVE_STATE_HIGH}

pattern GPIODLineActiveStateLow :: GPIODLineActiveState
pattern GPIODLineActiveStateLow =
  GPIODLineActiveState #{const GPIOD_LINE_ACTIVE_STATE_LOW}

{-# COMPLETE GPIODLineActiveStateHigh, GPIODLineActiveStateLow #-}

newtype GPIODLineBias = GPIODLineBias CInt
  deriving (Show, Eq, Ord, Storable)

pattern GPIODLineBiasAsIs :: GPIODLineBias
pattern GPIODLineBiasAsIs =
  GPIODLineBias #{const GPIOD_LINE_BIAS_AS_IS}

pattern GPIODLineBiasDisable :: GPIODLineBias
pattern GPIODLineBiasDisable =
  GPIODLineBias #{const GPIOD_LINE_BIAS_DISABLE}

pattern GPIODLineBiasPullUp :: GPIODLineBias
pattern GPIODLineBiasPullUp =
  GPIODLineBias #{const GPIOD_LINE_BIAS_PULL_UP}

pattern GPIODLineBiasPullDown :: GPIODLineBias
pattern GPIODLineBiasPullDown =
  GPIODLineBias #{const GPIOD_LINE_BIAS_PULL_DOWN}

{-# COMPLETE GPIODLineBiasAsIs, GPIODLineBiasDisable, GPIODLineBiasPullUp, GPIODLineBiasPullDown #-}

foreign import capi "gpiod.h"
  gpiod_line_offset :: Ptr GPIODLine -> IO CUInt

foreign import capi "gpiod.h"
  gpiod_line_name :: Ptr GPIODLine -> IO (ConstPtr CChar)

foreign import capi "gpiod.h"
  gpiod_line_consumer :: Ptr GPIODLine -> IO (ConstPtr CChar)

foreign import capi "gpiod.h"
  gpiod_line_direction :: Ptr GPIODLine -> IO GPIODLineDirection

foreign import capi "gpiod.h"
  gpiod_line_active_state :: Ptr GPIODLine -> IO GPIODLineActiveState

foreign import capi "gpiod.h"
  gpiod_line_bias :: Ptr GPIODLine -> IO GPIODLineBias

foreign import capi "gpiod.h"
  gpiod_line_is_used :: Ptr GPIODLine -> IO CBool

foreign import capi "gpiod.h"
  gpiod_line_is_open_drain :: Ptr GPIODLine -> IO CBool

foreign import capi "gpiod.h"
  gpiod_line_is_open_source :: Ptr GPIODLine -> IO CBool

foreign import capi "gpiod.h"
  gpiod_line_update :: Ptr GPIODLine -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_find :: ConstPtr CChar -> IO (Ptr GPIODLine)

foreign import capi "gpiod.h"
  gpiod_line_close_chip :: Ptr GPIODLine -> IO ()

foreign import capi "gpiod.h &gpiod_line_close_chip"
  gpiod_line_close_chip_p :: FunPtr (Ptr GPIODLine -> IO ())

foreign import capi "gpiod.h"
  gpiod_line_get_chip :: Ptr GPIODLine -> IO (Ptr GPIODChip)


-- * Line requests

newtype GPIODLineRequestType = GPIODLineRequestType CInt
  deriving (Show, Eq, Ord, Storable)

pattern GPIODLineRequestDirectionAsIs :: GPIODLineRequestType
pattern GPIODLineRequestDirectionAsIs =
  GPIODLineRequestType #{const GPIOD_LINE_REQUEST_DIRECTION_AS_IS}

pattern GPIODLineRequestDirectionInput :: GPIODLineRequestType
pattern GPIODLineRequestDirectionInput =
  GPIODLineRequestType #{const GPIOD_LINE_REQUEST_DIRECTION_INPUT}

pattern GPIODLineRequestDirectionOutput :: GPIODLineRequestType
pattern GPIODLineRequestDirectionOutput =
  GPIODLineRequestType #{const GPIOD_LINE_REQUEST_DIRECTION_OUTPUT}

pattern GPIODLineRequestEventFallingEdge :: GPIODLineRequestType
pattern GPIODLineRequestEventFallingEdge =
  GPIODLineRequestType #{const GPIOD_LINE_REQUEST_EVENT_FALLING_EDGE}

pattern GPIODLineRequestEventRisingEdge :: GPIODLineRequestType
pattern GPIODLineRequestEventRisingEdge =
  GPIODLineRequestType #{const GPIOD_LINE_REQUEST_EVENT_RISING_EDGE}

pattern GPIODLineRequestEventBothEdges :: GPIODLineRequestType
pattern GPIODLineRequestEventBothEdges =
  GPIODLineRequestType #{const GPIOD_LINE_REQUEST_EVENT_BOTH_EDGES}

{-# COMPLETE GPIODLineRequestDirectionAsIs, GPIODLineRequestDirectionInput, GPIODLineRequestDirectionOutput, GPIODLineRequestEventFallingEdge, GPIODLineRequestEventRisingEdge, GPIODLineRequestEventBothEdges #-}

newtype GPIODLineRequestFlag = GPIODLineRequestFlag CInt
  deriving (Show, Eq, Ord, Storable, Bits)

pattern GPIODLineRequestFlagOpenDrain :: GPIODLineRequestFlag
pattern GPIODLineRequestFlagOpenDrain =
  GPIODLineRequestFlag #{const GPIOD_LINE_REQUEST_FLAG_OPEN_DRAIN}

pattern GPIODLineRequestFlagOpenSource :: GPIODLineRequestFlag
pattern GPIODLineRequestFlagOpenSource =
  GPIODLineRequestFlag #{const GPIOD_LINE_REQUEST_FLAG_OPEN_SOURCE}

pattern GPIODLineRequestFlagActiveLow :: GPIODLineRequestFlag
pattern GPIODLineRequestFlagActiveLow =
  GPIODLineRequestFlag #{const GPIOD_LINE_REQUEST_FLAG_ACTIVE_LOW}

pattern GPIODLineRequestFlagBiasDisable :: GPIODLineRequestFlag
pattern GPIODLineRequestFlagBiasDisable =
  GPIODLineRequestFlag #{const GPIOD_LINE_REQUEST_FLAG_BIAS_DISABLE}

pattern GPIODLineRequestFlagBiasPullDown :: GPIODLineRequestFlag
pattern GPIODLineRequestFlagBiasPullDown =
  GPIODLineRequestFlag #{const GPIOD_LINE_REQUEST_FLAG_BIAS_PULL_DOWN}

pattern GPIODLineRequestFlagBiasPullUp :: GPIODLineRequestFlag
pattern GPIODLineRequestFlagBiasPullUp =
  GPIODLineRequestFlag #{const GPIOD_LINE_REQUEST_FLAG_BIAS_PULL_UP}

{-# COMPLETE GPIODLineRequestFlagOpenDrain, GPIODLineRequestFlagOpenSource, GPIODLineRequestFlagActiveLow, GPIODLineRequestFlagBiasDisable, GPIODLineRequestFlagBiasPullDown, GPIODLineRequestFlagBiasPullUp #-}

data GPIODLineRequestConfig = GPIODLineRequestConfig
  { consumer :: ConstPtr CChar
  , requestType :: GPIODLineRequestType
  , flags :: GPIODLineRequestFlag
  } deriving (Show, Eq, Ord)

instance Storable GPIODLineRequestConfig where
  sizeOf _ = #{size struct gpiod_line_request_config}
  alignment _ = #{alignment struct gpiod_line_request_config}
  peek p =
    GPIODLineRequestConfig <$> #{peek struct gpiod_line_request_config, consumer} p
                           <*> #{peek struct gpiod_line_request_config, request_type} p
                           <*> #{peek struct gpiod_line_request_config, flags} p
  poke p (GPIODLineRequestConfig c r f) = do
    #{poke struct gpiod_line_request_config, consumer} p c
    #{poke struct gpiod_line_request_config, request_type} p r
    #{poke struct gpiod_line_request_config, flags} p f

foreign import capi "gpiod.h"
  gpiod_line_request :: Ptr GPIODLine -> ConstPtr GPIODLineRequestConfig -> CInt -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_input :: Ptr GPIODLine -> ConstPtr CChar -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_output :: Ptr GPIODLine -> ConstPtr CChar -> CInt -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_rising_edge_events :: Ptr GPIODLine -> ConstPtr CChar -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_falling_edge_events :: Ptr GPIODLine -> ConstPtr CChar -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_both_edges_events :: Ptr GPIODLine -> ConstPtr CChar -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_input_flags :: Ptr GPIODLine -> ConstPtr CChar -> GPIODLineRequestFlag -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_output_flags :: Ptr GPIODLine -> ConstPtr CChar -> GPIODLineRequestFlag -> CInt -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_rising_edge_events_flags :: Ptr GPIODLine -> ConstPtr CChar -> GPIODLineRequestFlag -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_falling_edge_events_flags :: Ptr GPIODLine -> ConstPtr CChar -> GPIODLineRequestFlag -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_both_edges_events_flags :: Ptr GPIODLine -> ConstPtr CChar -> GPIODLineRequestFlag -> IO CInt


foreign import capi "gpiod.h"
  gpiod_line_request_bulk :: Ptr GPIODLine -> ConstPtr GPIODLineRequestConfig -> ConstPtr CInt -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_bulk_input :: Ptr GPIODLine -> ConstPtr CChar -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_bulk_output :: Ptr GPIODLine -> ConstPtr CChar -> ConstPtr CInt -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_bulk_rising_edge_events :: Ptr GPIODLine -> ConstPtr CChar -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_bulk_falling_edge_events :: Ptr GPIODLine -> ConstPtr CChar -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_bulk_both_edges_events :: Ptr GPIODLine -> ConstPtr CChar -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_bulk_input_flags :: Ptr GPIODLine -> ConstPtr CChar -> GPIODLineRequestFlag -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_bulk_output_flags :: Ptr GPIODLine -> ConstPtr CChar -> GPIODLineRequestFlag -> ConstPtr CInt -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_bulk_rising_edge_events_flags :: Ptr GPIODLine -> ConstPtr CChar -> GPIODLineRequestFlag -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_bulk_falling_edge_events_flags :: Ptr GPIODLine -> ConstPtr CChar -> GPIODLineRequestFlag -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_request_bulk_both_edges_events_flags :: Ptr GPIODLine -> ConstPtr CChar -> GPIODLineRequestFlag -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_release :: Ptr GPIODLine -> IO ()

foreign import capi "gpiod.h &gpiod_line_release"
  gpiod_line_release_p :: FunPtr (Ptr GPIODLine -> IO ())

foreign import capi "gpiod.h"
  gpiod_line_release_bulk :: Ptr GPIODLineBulk -> IO ()

foreign import capi "gpiod.h &gpiod_line_release_bulk"
  gpiod_line_release_bulk_p :: FunPtr (Ptr GPIODLineBulk -> IO ())

foreign import capi "gpiod.h"
  gpiod_line_is_requested :: Ptr GPIODLine -> IO Bool

foreign import capi "gpiod.h"
  gpiod_line_is_free :: Ptr GPIODLine -> IO Bool


-- * Reading and setting line values

foreign import capi "gpiod.h"
  gpiod_line_get_value :: Ptr GPIODLine -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_set_value :: Ptr GPIODLine -> CInt -> IO CInt

-- * Setting line configuration

-- * Line events handling

newtype GPIODLineEventType = GPIODLineEventType CInt
  deriving (Show, Eq, Ord, Storable)

pattern GPIODLineEventRisingEdge :: GPIODLineEventType
pattern GPIODLineEventRisingEdge =
  GPIODLineEventType #{const GPIOD_LINE_EVENT_RISING_EDGE}

pattern GPIODLineEventFallingEdge :: GPIODLineEventType
pattern GPIODLineEventFallingEdge =
  GPIODLineEventType #{const GPIOD_LINE_EVENT_FALLING_EDGE}

{-# COMPLETE GPIODLineEventRisingEdge, GPIODLineEventFallingEdge #-}

data GPIODLineEvent = GPIODLineEvent
  { timespec :: TimeSpec
  , eventType :: GPIODLineEventType
  } deriving (Show, Eq, Ord)

instance Storable GPIODLineEvent where
  sizeOf _ = #{size struct gpiod_line_event}
  alignment _ = #{alignment struct gpiod_line_event}
  peek p =
    GPIODLineEvent <$> #{peek struct gpiod_line_event, ts} p
                   <*> #{peek struct gpiod_line_event, event_type} p
  poke p le = do
    #{poke struct gpiod_line_event, ts} p (timespec le)
    #{poke struct gpiod_line_event, event_type} p (eventType le)

foreign import capi "gpiod.h"
  gpiod_line_event_wait :: Ptr GPIODLine -> ConstPtr TimeSpec -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_event_wait_bulk :: Ptr GPIODLineBulk -> ConstPtr TimeSpec -> Ptr GPIODLineBulk -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_event_read :: Ptr GPIODLineBulk -> Ptr GPIODLineEvent -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_event_read_multiple :: Ptr GPIODLineBulk -> Ptr GPIODLineEvent -> CUInt -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_event_get_fd :: Ptr GPIODLine -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_event_read_fd :: CInt -> Ptr GPIODLineEvent -> IO CInt

foreign import capi "gpiod.h"
  gpiod_line_event_read_fd_multiple:: CInt -> Ptr GPIODLineEvent -> CUInt -> IO CInt


-- * Iterators for GPIO chips and lines

data GPIODChipIter

foreign import capi "gpiod.h"
  gpiod_chip_iter_new :: IO (Ptr GPIODChipIter)

foreign import capi "gpiod.h"
  gpiod_chip_iter_free :: Ptr GPIODChipIter -> IO ()

foreign import capi "gpiod.h &gpiod_chip_iter_free"
  gpiod_chip_iter_free_p :: FunPtr (Ptr GPIODChipIter -> IO ())

foreign import capi "gpiod.h"
  gpiod_chip_iter_free_noclose :: Ptr GPIODChipIter -> IO ()

foreign import capi "gpiod.h &gpiod_chip_iter_free_noclose"
  gpiod_chip_iter_free_noclose_p :: FunPtr (Ptr GPIODChipIter -> IO ())

foreign import capi "gpiod.h"
  gpiod_chip_iter_next :: Ptr GPIODChipIter -> IO (Ptr GPIODChip)

foreign import capi "gpiod.h"
  gpiod_chip_iter_next_noclose :: Ptr GPIODChipIter -> IO (Ptr GPIODChip)


data GPIODLineIter

foreign import capi "gpiod.h"
  gpiod_line_iter_new :: Ptr GPIODChip -> IO (Ptr GPIODLineIter)

foreign import capi "gpiod.h"
  gpiod_line_iter_free :: Ptr GPIODLineIter -> IO ()

foreign import capi "gpiod.h &gpiod_line_iter_free"
  gpiod_line_iter_free_p :: FunPtr (Ptr GPIODLineIter -> IO ())

foreign import capi "gpiod.h"
  gpiod_line_iter_next :: Ptr GPIODLineIter -> IO (Ptr GPIODLine)


-- * Version

foreign import capi "gpiod.h"
  gpiod_version_string :: IO (ConstPtr CChar)
