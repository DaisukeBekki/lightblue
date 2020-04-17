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

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,80) ([9728,49160,260,0,128,38912,93,2,0,1,0,2086,0,8192,16384,0,16384,45824,24579,118,3788,55680,12289,59,1894,60608,38912,29,16,64,0,16,0,0,0,0,0,0,0,0,0,3788,0,4,16,32,60608,0,32,0,30304,0,32768,473,16384,0,16386,812,0,0,0,64,12996,22656,4102,203,6498,11328,34819,101,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_expr","terms","words","formula","term","int","coron","file","per","word","connective","predicates","and","comma","period","rbracket","lbracket","fof","%eof"]
        bit_start = st * 21
        bit_end = (st + 1) * 21
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..20]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (10) = happyShift action_3
action_0 (11) = happyShift action_4
action_0 (14) = happyShift action_5
action_0 (20) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (7) = happyGoto action_8
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (10) = happyShift action_3
action_1 (11) = happyShift action_4
action_1 (14) = happyShift action_5
action_1 (20) = happyShift action_6
action_1 (7) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyFail (happyExpListPerState 2)

action_3 (9) = happyShift action_21
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (8) = happyShift action_13
action_4 (9) = happyShift action_14
action_4 (12) = happyShift action_15
action_4 (13) = happyShift action_16
action_4 (15) = happyShift action_17
action_4 (16) = happyShift action_18
action_4 (17) = happyShift action_19
action_4 (19) = happyShift action_20
action_4 (5) = happyGoto action_12
action_4 _ = happyReduce_36

action_5 (9) = happyShift action_11
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (19) = happyShift action_10
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (21) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (10) = happyShift action_3
action_8 (11) = happyShift action_4
action_8 (14) = happyShift action_5
action_8 (20) = happyShift action_6
action_8 (4) = happyGoto action_9
action_8 (7) = happyGoto action_8
action_8 _ = happyReduce_1

action_9 _ = happyReduce_2

action_10 (12) = happyShift action_33
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (8) = happyShift action_32
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (19) = happyShift action_31
action_12 _ = happyReduce_34

action_13 (8) = happyShift action_13
action_13 (9) = happyShift action_14
action_13 (12) = happyShift action_15
action_13 (13) = happyShift action_16
action_13 (15) = happyShift action_17
action_13 (16) = happyShift action_18
action_13 (17) = happyShift action_19
action_13 (5) = happyGoto action_30
action_13 _ = happyReduce_4

action_14 (8) = happyShift action_13
action_14 (9) = happyShift action_14
action_14 (12) = happyShift action_15
action_14 (13) = happyShift action_16
action_14 (15) = happyShift action_17
action_14 (16) = happyShift action_18
action_14 (17) = happyShift action_19
action_14 (5) = happyGoto action_29
action_14 _ = happyReduce_5

action_15 (8) = happyShift action_13
action_15 (9) = happyShift action_14
action_15 (12) = happyShift action_15
action_15 (13) = happyShift action_16
action_15 (15) = happyShift action_17
action_15 (16) = happyShift action_18
action_15 (17) = happyShift action_19
action_15 (5) = happyGoto action_28
action_15 _ = happyReduce_3

action_16 (8) = happyShift action_13
action_16 (9) = happyShift action_14
action_16 (12) = happyShift action_15
action_16 (13) = happyShift action_16
action_16 (15) = happyShift action_17
action_16 (16) = happyShift action_18
action_16 (17) = happyShift action_19
action_16 (5) = happyGoto action_27
action_16 _ = happyReduce_8

action_17 (8) = happyShift action_13
action_17 (9) = happyShift action_14
action_17 (12) = happyShift action_15
action_17 (13) = happyShift action_16
action_17 (15) = happyShift action_17
action_17 (16) = happyShift action_18
action_17 (17) = happyShift action_19
action_17 (5) = happyGoto action_26
action_17 _ = happyReduce_9

action_18 (8) = happyShift action_13
action_18 (9) = happyShift action_14
action_18 (12) = happyShift action_15
action_18 (13) = happyShift action_16
action_18 (15) = happyShift action_17
action_18 (16) = happyShift action_18
action_18 (17) = happyShift action_19
action_18 (5) = happyGoto action_25
action_18 _ = happyReduce_6

action_19 (8) = happyShift action_13
action_19 (9) = happyShift action_14
action_19 (12) = happyShift action_15
action_19 (13) = happyShift action_16
action_19 (15) = happyShift action_17
action_19 (16) = happyShift action_18
action_19 (17) = happyShift action_19
action_19 (5) = happyGoto action_24
action_19 _ = happyReduce_7

action_20 (8) = happyShift action_13
action_20 (9) = happyShift action_14
action_20 (12) = happyShift action_15
action_20 (13) = happyShift action_16
action_20 (15) = happyShift action_17
action_20 (16) = happyShift action_18
action_20 (17) = happyShift action_19
action_20 (5) = happyGoto action_23
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (12) = happyShift action_22
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (9) = happyShift action_38
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (18) = happyShift action_37
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_12

action_25 _ = happyReduce_13

action_26 _ = happyReduce_10

action_27 _ = happyReduce_15

action_28 _ = happyReduce_14

action_29 _ = happyReduce_11

action_30 _ = happyReduce_16

action_31 (8) = happyShift action_13
action_31 (9) = happyShift action_14
action_31 (12) = happyShift action_15
action_31 (13) = happyShift action_16
action_31 (15) = happyShift action_17
action_31 (16) = happyShift action_18
action_31 (17) = happyShift action_19
action_31 (5) = happyGoto action_36
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (19) = happyShift action_35
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (16) = happyShift action_34
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (12) = happyShift action_42
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (8) = happyShift action_13
action_35 (9) = happyShift action_14
action_35 (12) = happyShift action_15
action_35 (13) = happyShift action_16
action_35 (15) = happyShift action_17
action_35 (16) = happyShift action_18
action_35 (17) = happyShift action_19
action_35 (5) = happyGoto action_41
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (18) = happyShift action_40
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_33

action_38 (8) = happyShift action_13
action_38 (9) = happyShift action_14
action_38 (12) = happyShift action_15
action_38 (13) = happyShift action_16
action_38 (15) = happyShift action_17
action_38 (16) = happyShift action_18
action_38 (17) = happyShift action_19
action_38 (5) = happyGoto action_39
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_29

action_40 (8) = happyShift action_13
action_40 (9) = happyShift action_14
action_40 (12) = happyShift action_15
action_40 (13) = happyShift action_16
action_40 (15) = happyShift action_17
action_40 (16) = happyShift action_18
action_40 (17) = happyShift action_19
action_40 (5) = happyGoto action_45
action_40 _ = happyReduce_32

action_41 (18) = happyShift action_44
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (16) = happyShift action_43
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (8) = happyShift action_47
action_43 (12) = happyShift action_48
action_43 (13) = happyShift action_49
action_43 (15) = happyShift action_50
action_43 (18) = happyShift action_51
action_43 (19) = happyShift action_52
action_43 (6) = happyGoto action_46
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_30

action_45 _ = happyReduce_31

action_46 (17) = happyShift action_59
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (8) = happyShift action_47
action_47 (12) = happyShift action_48
action_47 (13) = happyShift action_49
action_47 (15) = happyShift action_50
action_47 (18) = happyShift action_51
action_47 (19) = happyShift action_52
action_47 (6) = happyGoto action_58
action_47 _ = happyReduce_18

action_48 (8) = happyShift action_47
action_48 (12) = happyShift action_48
action_48 (13) = happyShift action_49
action_48 (15) = happyShift action_50
action_48 (18) = happyShift action_51
action_48 (19) = happyShift action_52
action_48 (6) = happyGoto action_57
action_48 _ = happyReduce_17

action_49 (8) = happyShift action_47
action_49 (12) = happyShift action_48
action_49 (13) = happyShift action_49
action_49 (15) = happyShift action_50
action_49 (18) = happyShift action_51
action_49 (19) = happyShift action_52
action_49 (6) = happyGoto action_56
action_49 _ = happyReduce_19

action_50 (8) = happyShift action_47
action_50 (12) = happyShift action_48
action_50 (13) = happyShift action_49
action_50 (15) = happyShift action_50
action_50 (18) = happyShift action_51
action_50 (19) = happyShift action_52
action_50 (6) = happyGoto action_55
action_50 _ = happyReduce_20

action_51 (8) = happyShift action_47
action_51 (12) = happyShift action_48
action_51 (13) = happyShift action_49
action_51 (15) = happyShift action_50
action_51 (18) = happyShift action_51
action_51 (19) = happyShift action_52
action_51 (6) = happyGoto action_54
action_51 _ = happyReduce_26

action_52 (8) = happyShift action_47
action_52 (12) = happyShift action_48
action_52 (13) = happyShift action_49
action_52 (15) = happyShift action_50
action_52 (18) = happyShift action_51
action_52 (19) = happyShift action_52
action_52 (6) = happyGoto action_53
action_52 _ = happyReduce_25

action_53 _ = happyReduce_27

action_54 _ = happyReduce_28

action_55 _ = happyReduce_21

action_56 _ = happyReduce_23

action_57 _ = happyReduce_22

action_58 _ = happyReduce_24

action_59 _ = happyReduce_35

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
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
		 ([]
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
	(HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn6
		 ((show happy_var_1) : happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  6 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn6
		 (["("]
	)

happyReduce_26 = happySpecReduce_1  6 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn6
		 ([")"]
	)

happyReduce_27 = happySpecReduce_2  6 happyReduction_27
happyReduction_27 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 ("(" : happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  6 happyReduction_28
happyReduction_28 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (")" : happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 5 7 happyReduction_29
happyReduction_29 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (File happy_var_3 (L.concat happy_var_5)
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 6 7 happyReduction_30
happyReduction_30 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PreNum happy_var_3
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 6 7 happyReduction_31
happyReduction_31 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Sout ""
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 5 7 happyReduction_32
happyReduction_32 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Sout ""
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 4 7 happyReduction_33
happyReduction_33 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Sout ""
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_2  7 happyReduction_34
happyReduction_34 _
	_
	 =  HappyAbsSyn7
		 (Sout ""
	)

happyReduce_35 = happyReduce 8 7 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Formula "fof"  happy_var_3 happy_var_5 (L.init $ L.concat happy_var_7)
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_1  7 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn7
		 (Sout ""
	)

happyNewToken action sts stk [] =
	action 21 21 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 8;
	TokenCoron -> cont 9;
	TokenFile -> cont 10;
	TokenHead -> cont 11;
	TokenWord happy_dollar_dollar -> cont 12;
	TokenConne happy_dollar_dollar -> cont 13;
	TokenPreNum -> cont 14;
	TokenAnd -> cont 15;
	TokenComma -> cont 16;
	TokenPeriod -> cont 17;
	TokenRBracket -> cont 18;
	TokenLBracket -> cont 19;
	TokenFOF -> cont 20;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 21 tk tks = happyError' (tks, explist)
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

