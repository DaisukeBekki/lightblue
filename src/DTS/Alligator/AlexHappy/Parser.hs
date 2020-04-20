{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DTS.Alligator.AlexHappy.Parser (
  parseExpr,
) where

import DTS.Alligator.AlexHappy.Lexer
import DTS.Alligator.AlexHappy.Syntax
import qualified Data.List as L           -- base

import Control.Monad.Except
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,152) ([19456,16,1043,0,2048,0,4019,128,0,128,0,19456,16,0,256,1024,0,4019,60608,3,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,32768,0,0,0,0,3251,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,16384,0,1,3788,45824,3,0,16384,0,4,3251,0,0,32,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_expr","terms","words","formula","others","term","int","coron","file","per","word","connective","predicates","and","comma","period","rbracket","lbracket","fof","%eof"]
        bit_start = st * 22
        bit_end = (st + 1) * 22
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..21]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (11) = happyShift action_3
action_0 (12) = happyShift action_4
action_0 (15) = happyShift action_5
action_0 (21) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (8) = happyGoto action_8
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (11) = happyShift action_3
action_1 (12) = happyShift action_4
action_1 (15) = happyShift action_5
action_1 (21) = happyShift action_6
action_1 (8) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyFail (happyExpListPerState 2)

action_3 (10) = happyShift action_24
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (9) = happyShift action_15
action_4 (10) = happyShift action_16
action_4 (13) = happyShift action_17
action_4 (14) = happyShift action_18
action_4 (16) = happyShift action_19
action_4 (17) = happyShift action_20
action_4 (18) = happyShift action_21
action_4 (19) = happyShift action_22
action_4 (20) = happyShift action_23
action_4 (5) = happyGoto action_12
action_4 (6) = happyGoto action_13
action_4 (7) = happyGoto action_14
action_4 _ = happyReduce_38

action_5 (10) = happyShift action_11
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (20) = happyShift action_10
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (22) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (11) = happyShift action_3
action_8 (12) = happyShift action_4
action_8 (15) = happyShift action_5
action_8 (21) = happyShift action_6
action_8 (4) = happyGoto action_9
action_8 (8) = happyGoto action_8
action_8 _ = happyReduce_1

action_9 _ = happyReduce_2

action_10 (13) = happyShift action_53
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (9) = happyShift action_52
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (9) = happyShift action_15
action_12 (10) = happyShift action_16
action_12 (13) = happyShift action_17
action_12 (14) = happyShift action_18
action_12 (16) = happyShift action_19
action_12 (17) = happyShift action_20
action_12 (18) = happyShift action_21
action_12 (19) = happyShift action_22
action_12 (20) = happyShift action_23
action_12 (5) = happyGoto action_12
action_12 (6) = happyGoto action_13
action_12 (7) = happyGoto action_51
action_12 _ = happyReduce_31

action_13 (9) = happyShift action_15
action_13 (10) = happyShift action_16
action_13 (13) = happyShift action_17
action_13 (14) = happyShift action_18
action_13 (16) = happyShift action_19
action_13 (17) = happyShift action_20
action_13 (18) = happyShift action_21
action_13 (19) = happyShift action_22
action_13 (20) = happyShift action_23
action_13 (5) = happyGoto action_12
action_13 (6) = happyGoto action_13
action_13 (7) = happyGoto action_50
action_13 _ = happyReduce_30

action_14 _ = happyReduce_36

action_15 (9) = happyShift action_15
action_15 (10) = happyShift action_16
action_15 (11) = happyReduce_18
action_15 (12) = happyReduce_18
action_15 (13) = happyShift action_17
action_15 (14) = happyShift action_18
action_15 (15) = happyReduce_18
action_15 (16) = happyShift action_19
action_15 (17) = happyShift action_20
action_15 (18) = happyShift action_21
action_15 (19) = happyShift action_22
action_15 (20) = happyShift action_23
action_15 (21) = happyReduce_18
action_15 (22) = happyReduce_18
action_15 (5) = happyGoto action_48
action_15 (6) = happyGoto action_49
action_15 _ = happyReduce_18

action_16 (9) = happyShift action_15
action_16 (10) = happyShift action_16
action_16 (13) = happyShift action_17
action_16 (14) = happyShift action_18
action_16 (16) = happyShift action_19
action_16 (17) = happyShift action_20
action_16 (18) = happyShift action_21
action_16 (19) = happyShift action_22
action_16 (20) = happyShift action_23
action_16 (5) = happyGoto action_46
action_16 (6) = happyGoto action_47
action_16 _ = happyReduce_5

action_17 (9) = happyShift action_15
action_17 (10) = happyShift action_16
action_17 (11) = happyReduce_17
action_17 (12) = happyReduce_17
action_17 (13) = happyShift action_17
action_17 (14) = happyShift action_18
action_17 (15) = happyReduce_17
action_17 (16) = happyShift action_19
action_17 (17) = happyShift action_20
action_17 (18) = happyShift action_21
action_17 (19) = happyShift action_22
action_17 (20) = happyShift action_23
action_17 (21) = happyReduce_17
action_17 (22) = happyReduce_17
action_17 (5) = happyGoto action_44
action_17 (6) = happyGoto action_45
action_17 _ = happyReduce_17

action_18 (9) = happyShift action_15
action_18 (10) = happyShift action_16
action_18 (11) = happyReduce_19
action_18 (12) = happyReduce_19
action_18 (13) = happyShift action_17
action_18 (14) = happyShift action_18
action_18 (15) = happyReduce_19
action_18 (16) = happyShift action_19
action_18 (17) = happyShift action_20
action_18 (18) = happyShift action_21
action_18 (19) = happyShift action_22
action_18 (20) = happyShift action_23
action_18 (21) = happyReduce_19
action_18 (22) = happyReduce_19
action_18 (5) = happyGoto action_42
action_18 (6) = happyGoto action_43
action_18 _ = happyReduce_19

action_19 (9) = happyShift action_15
action_19 (10) = happyShift action_16
action_19 (11) = happyReduce_20
action_19 (12) = happyReduce_20
action_19 (13) = happyShift action_17
action_19 (14) = happyShift action_18
action_19 (15) = happyReduce_20
action_19 (16) = happyShift action_19
action_19 (17) = happyShift action_20
action_19 (18) = happyShift action_21
action_19 (19) = happyShift action_22
action_19 (20) = happyShift action_23
action_19 (21) = happyReduce_20
action_19 (22) = happyReduce_20
action_19 (5) = happyGoto action_40
action_19 (6) = happyGoto action_41
action_19 _ = happyReduce_20

action_20 (9) = happyShift action_34
action_20 (10) = happyShift action_35
action_20 (13) = happyShift action_36
action_20 (14) = happyShift action_37
action_20 (16) = happyShift action_38
action_20 (17) = happyShift action_20
action_20 (18) = happyShift action_21
action_20 (5) = happyGoto action_39
action_20 _ = happyReduce_6

action_21 (9) = happyShift action_34
action_21 (10) = happyShift action_35
action_21 (13) = happyShift action_36
action_21 (14) = happyShift action_37
action_21 (16) = happyShift action_38
action_21 (17) = happyShift action_20
action_21 (18) = happyShift action_21
action_21 (5) = happyGoto action_33
action_21 _ = happyReduce_7

action_22 (9) = happyShift action_27
action_22 (10) = happyShift action_28
action_22 (13) = happyShift action_29
action_22 (14) = happyShift action_30
action_22 (16) = happyShift action_31
action_22 (19) = happyShift action_22
action_22 (20) = happyShift action_23
action_22 (6) = happyGoto action_32
action_22 _ = happyReduce_27

action_23 (9) = happyShift action_27
action_23 (10) = happyShift action_28
action_23 (13) = happyShift action_29
action_23 (14) = happyShift action_30
action_23 (16) = happyShift action_31
action_23 (19) = happyShift action_22
action_23 (20) = happyShift action_23
action_23 (6) = happyGoto action_26
action_23 _ = happyReduce_26

action_24 (13) = happyShift action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (10) = happyShift action_56
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_28

action_27 (9) = happyShift action_27
action_27 (10) = happyShift action_28
action_27 (13) = happyShift action_29
action_27 (14) = happyShift action_30
action_27 (16) = happyShift action_31
action_27 (19) = happyShift action_22
action_27 (20) = happyShift action_23
action_27 (6) = happyGoto action_49
action_27 _ = happyReduce_18

action_28 (9) = happyShift action_27
action_28 (10) = happyShift action_28
action_28 (13) = happyShift action_29
action_28 (14) = happyShift action_30
action_28 (16) = happyShift action_31
action_28 (19) = happyShift action_22
action_28 (20) = happyShift action_23
action_28 (6) = happyGoto action_47
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (9) = happyShift action_27
action_29 (10) = happyShift action_28
action_29 (13) = happyShift action_29
action_29 (14) = happyShift action_30
action_29 (16) = happyShift action_31
action_29 (19) = happyShift action_22
action_29 (20) = happyShift action_23
action_29 (6) = happyGoto action_45
action_29 _ = happyReduce_17

action_30 (9) = happyShift action_27
action_30 (10) = happyShift action_28
action_30 (13) = happyShift action_29
action_30 (14) = happyShift action_30
action_30 (16) = happyShift action_31
action_30 (19) = happyShift action_22
action_30 (20) = happyShift action_23
action_30 (6) = happyGoto action_43
action_30 _ = happyReduce_19

action_31 (9) = happyShift action_27
action_31 (10) = happyShift action_28
action_31 (13) = happyShift action_29
action_31 (14) = happyShift action_30
action_31 (16) = happyShift action_31
action_31 (19) = happyShift action_22
action_31 (20) = happyShift action_23
action_31 (6) = happyGoto action_41
action_31 _ = happyReduce_20

action_32 _ = happyReduce_29

action_33 _ = happyReduce_12

action_34 (9) = happyShift action_34
action_34 (10) = happyShift action_35
action_34 (13) = happyShift action_36
action_34 (14) = happyShift action_37
action_34 (16) = happyShift action_38
action_34 (17) = happyShift action_20
action_34 (18) = happyShift action_21
action_34 (5) = happyGoto action_48
action_34 _ = happyReduce_4

action_35 (9) = happyShift action_34
action_35 (10) = happyShift action_35
action_35 (13) = happyShift action_36
action_35 (14) = happyShift action_37
action_35 (16) = happyShift action_38
action_35 (17) = happyShift action_20
action_35 (18) = happyShift action_21
action_35 (5) = happyGoto action_46
action_35 _ = happyReduce_5

action_36 (9) = happyShift action_34
action_36 (10) = happyShift action_35
action_36 (13) = happyShift action_36
action_36 (14) = happyShift action_37
action_36 (16) = happyShift action_38
action_36 (17) = happyShift action_20
action_36 (18) = happyShift action_21
action_36 (5) = happyGoto action_44
action_36 _ = happyReduce_3

action_37 (9) = happyShift action_34
action_37 (10) = happyShift action_35
action_37 (13) = happyShift action_36
action_37 (14) = happyShift action_37
action_37 (16) = happyShift action_38
action_37 (17) = happyShift action_20
action_37 (18) = happyShift action_21
action_37 (5) = happyGoto action_42
action_37 _ = happyReduce_8

action_38 (9) = happyShift action_34
action_38 (10) = happyShift action_35
action_38 (13) = happyShift action_36
action_38 (14) = happyShift action_37
action_38 (16) = happyShift action_38
action_38 (17) = happyShift action_20
action_38 (18) = happyShift action_21
action_38 (5) = happyGoto action_40
action_38 _ = happyReduce_9

action_39 _ = happyReduce_13

action_40 _ = happyReduce_10

action_41 _ = happyReduce_21

action_42 _ = happyReduce_15

action_43 _ = happyReduce_23

action_44 _ = happyReduce_14

action_45 _ = happyReduce_22

action_46 _ = happyReduce_11

action_47 _ = happyReduce_24

action_48 _ = happyReduce_16

action_49 _ = happyReduce_25

action_50 _ = happyReduce_32

action_51 _ = happyReduce_33

action_52 (20) = happyShift action_55
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (17) = happyShift action_54
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (13) = happyShift action_59
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (9) = happyShift action_34
action_55 (10) = happyShift action_35
action_55 (13) = happyShift action_36
action_55 (14) = happyShift action_37
action_55 (16) = happyShift action_38
action_55 (17) = happyShift action_20
action_55 (18) = happyShift action_21
action_55 (5) = happyGoto action_58
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (9) = happyShift action_34
action_56 (10) = happyShift action_35
action_56 (13) = happyShift action_36
action_56 (14) = happyShift action_37
action_56 (16) = happyShift action_38
action_56 (17) = happyShift action_20
action_56 (18) = happyShift action_21
action_56 (5) = happyGoto action_57
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_34

action_58 (19) = happyShift action_61
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (17) = happyShift action_60
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (9) = happyShift action_27
action_60 (10) = happyShift action_28
action_60 (13) = happyShift action_29
action_60 (14) = happyShift action_30
action_60 (16) = happyShift action_31
action_60 (19) = happyShift action_22
action_60 (20) = happyShift action_23
action_60 (6) = happyGoto action_62
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_35

action_62 (18) = happyShift action_63
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_37

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn5
		 ([(show happy_var_1)]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn5
		 ([":"]
	)

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn5
		 ([]
	)

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn5
		 ([]
	)

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyTerminal (TokenConne happy_var_1))
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn5
		 (["&"]
	)

happyReduce_10 = happySpecReduce_2  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 ("&" : happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenConne happy_var_1))
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  5 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn5
		 ((show happy_var_1) : happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  6 happyReduction_17
happyReduction_17 (HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn6
		 ([(show happy_var_1)]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  6 happyReduction_19
happyReduction_19 (HappyTerminal (TokenConne happy_var_1))
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  6 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn6
		 (["&"]
	)

happyReduce_21 = happySpecReduce_2  6 happyReduction_21
happyReduction_21 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 ("&" : happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  6 happyReduction_22
happyReduction_22 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  6 happyReduction_23
happyReduction_23 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal (TokenConne happy_var_1))
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  6 happyReduction_24
happyReduction_24 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (":" : happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  6 happyReduction_25
happyReduction_25 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn6
		 ((show happy_var_1) : happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  6 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn6
		 (["("]
	)

happyReduce_27 = happySpecReduce_1  6 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn6
		 ([")"]
	)

happyReduce_28 = happySpecReduce_2  6 happyReduction_28
happyReduction_28 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 ("(" : happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  6 happyReduction_29
happyReduction_29 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (")" : happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  7 happyReduction_30
happyReduction_30 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  7 happyReduction_31
happyReduction_31 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  7 happyReduction_32
happyReduction_32 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  7 happyReduction_33
happyReduction_33 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 5 8 happyReduction_34
happyReduction_34 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (File happy_var_3 (L.concat happy_var_5)
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 6 8 happyReduction_35
happyReduction_35 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PreNum happy_var_3
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_2  8 happyReduction_36
happyReduction_36 _
	_
	 =  HappyAbsSyn8
		 (Sout ""
	)

happyReduce_37 = happyReduce 8 8 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Formula "fof"  happy_var_3 happy_var_5 (L.init $ L.concat happy_var_7)
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_1  8 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn8
		 (Sout ""
	)

happyNewToken action sts stk [] =
	action 22 22 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 9;
	TokenCoron -> cont 10;
	TokenFile -> cont 11;
	TokenHead -> cont 12;
	TokenWord happy_dollar_dollar -> cont 13;
	TokenConne happy_dollar_dollar -> cont 14;
	TokenPreNum -> cont 15;
	TokenAnd -> cont 16;
	TokenComma -> cont 17;
	TokenPeriod -> cont 18;
	TokenRBracket -> cont 19;
	TokenLBracket -> cont 20;
	TokenFOF -> cont 21;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 22 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Except String a -> (a -> Except String b) -> Except String b
happyThen = ((>>=))
happyReturn :: () => a -> Except String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Except String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Except String a
happyError' = (\(tokens, _) -> parseError tokens)
expr tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String [Expr]
parseExpr input =
  let tokenStream = scanTokens input in
  runExcept (expr tokenStream)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 15 "<built-in>" #-}
{-# LINE 1 "/usr/local/Cellar/ghc/8.6.5/lib/ghc-8.6.5/include/ghcversion.h" #-}
















{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/var/folders/2s/x156s9rn0dbb07ymqyg052g40000gn/T/ghc37662_0/ghc_2.h" #-}

















































































































































































































































{-# LINE 17 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 










{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList








{-# LINE 65 "templates/GenericTemplate.hs" #-}


{-# LINE 75 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action


{-# LINE 137 "templates/GenericTemplate.hs" #-}


{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

