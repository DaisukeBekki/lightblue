{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "Lexer.x" #-}

module Lexer (
  Token(..),
  scanTokens
) where

import Syntax

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Array.Base (unsafeAt)
#else
import Array
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.






import Data.Word (Word8)
















import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type
















































































-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.














-- -----------------------------------------------------------------------------
-- Default monad
















































































































-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)







































































































-- -----------------------------------------------------------------------------
-- Basic wrapper


type AlexInput = (Char,[Byte],String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',[],str)
  where go inp__@(_,_bs,s) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act (take len s) : go inp__'

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (_,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode c of
                             (b:bs) -> Just (b, (c, bs, s))
                             [] -> Nothing



-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version
































-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.













-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version














-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.














alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0 :: Int, 10)
  [ -8
  , -3
  , 2
  , 76
  , 151
  , 0
  , 170
  , 254
  , 338
  , 422
  , 506
  ]

alex_table :: Array Int Int
alex_table = listArray (0 :: Int, 761)
  [ 0
  , 2
  , 1
  , 2
  , 2
  , 2
  , 2
  , 2
  , 2
  , 2
  , 2
  , 2
  , 2
  , 2
  , 2
  , 2
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 2
  , 0
  , 0
  , 0
  , 0
  , 2
  , 0
  , 0
  , 0
  , 0
  , 2
  , 0
  , 0
  , 0
  , 0
  , 0
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 0
  , 0
  , 0
  , 5
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 8
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 7
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 7
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 9
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 7
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 7
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 6
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 7
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 10
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 7
  , 0
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 3
  , 7
  , 7
  , 7
  , 7
  , 7
  , 7
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_check :: Array Int Int
alex_check = listArray (0 :: Int, 761)
  [ -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , 9
  , 10
  , 11
  , 12
  , 13
  , 9
  , 10
  , 11
  , 12
  , 13
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 32
  , -1
  , -1
  , -1
  , -1
  , 32
  , -1
  , -1
  , -1
  , -1
  , 32
  , -1
  , -1
  , -1
  , -1
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , 61
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 39
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 39
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 39
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 39
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 39
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 39
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , -1
  , -1
  , -1
  , -1
  , 95
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_deflt :: Array Int Int
alex_deflt = listArray (0 :: Int, 10)
  [ -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_accept = listArray (0 :: Int, 10)
  [ AlexAccNone
  , AlexAccSkip
  , AlexAccSkip
  , AlexAcc 7
  , AlexAcc 6
  , AlexAcc 5
  , AlexAcc 4
  , AlexAcc 3
  , AlexAcc 2
  , AlexAcc 1
  , AlexAcc 0
  ]

alex_actions = array (0 :: Int, 8)
  [ (7,alex_action_2)
  , (6,alex_action_3)
  , (5,alex_action_4)
  , (4,alex_action_5)
  , (3,alex_action_5)
  , (2,alex_action_5)
  , (1,alex_action_5)
  , (0,alex_action_5)
  ]

{-# LINE 26 "Lexer.x" #-}


data Token 
  = TokenNum Int
  | TokenSym String
  | TokenPrint
  | TokenEq
  | TokenEOF
  deriving (Eq,Show)

scanTokens = alexScanTokens


alex_action_2 =  \s -> TokenPrint 
alex_action_3 =  \s -> TokenNum (read s) 
alex_action_4 =  \s -> TokenEq 
alex_action_5 =  \s -> TokenSym s 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine































































alexIndexInt16OffAddr arr off = arr ! off




















alexIndexInt32OffAddr arr off = arr ! off











quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ (sc)
  = alexScanUser undefined input__ (sc)

alexScanUser user__ input__ (sc)
  = case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->



                                   AlexEOF
      Just _ ->



                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->



    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->



    AlexToken input__''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->



      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset

                new_s = if (offset >= (0)) && (check == ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            (-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ (len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ (len)

        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastAcc a input__ (len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastSkip input__ (len)
           | otherwise
           = check_accs rest


data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip

  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr ! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.

