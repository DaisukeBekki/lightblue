{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DTS.Alligator.AlexHappy.Parserf (
  parseExpr,
) where

import DTS.Alligator.AlexHappy.Lexerf
import DTS.Alligator.AlexHappy.Syntaxf
import qualified Data.List as L           -- base

import Control.Monad.Except
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,67) ([45152,8240,0,0,61440,1,2822,3,0,0,24768,97,512,0,4097,0,8,63488,4,0,49536,49346,24928,45152,12336,6232,11288,12,0,0,0,0,0,0,0,0,32768,0,512,8192,0,64,2,0,49168,24928,0,12288,6232,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_expr","formula","vars","neg","word","biOp","and","or","imp","equiv","top","bot","rbracket","lbracket","rrbracket","rlbracket","coron","comma","all","exists","%eof"]
        bit_start = st * 23
        bit_end = (st + 1) * 23
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..22]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (6) = happyShift action_4
action_0 (7) = happyShift action_2
action_0 (13) = happyShift action_5
action_0 (14) = happyShift action_6
action_0 (16) = happyShift action_7
action_0 (21) = happyShift action_8
action_0 (22) = happyShift action_9
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (7) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (8) = happyShift action_14
action_3 (9) = happyShift action_15
action_3 (10) = happyShift action_16
action_3 (11) = happyShift action_17
action_3 (12) = happyShift action_18
action_3 (23) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (6) = happyShift action_4
action_4 (7) = happyShift action_2
action_4 (13) = happyShift action_5
action_4 (14) = happyShift action_6
action_4 (16) = happyShift action_7
action_4 (21) = happyShift action_8
action_4 (22) = happyShift action_9
action_4 (4) = happyGoto action_13
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_2

action_6 _ = happyReduce_3

action_7 (6) = happyShift action_4
action_7 (7) = happyShift action_2
action_7 (13) = happyShift action_5
action_7 (14) = happyShift action_6
action_7 (16) = happyShift action_7
action_7 (21) = happyShift action_8
action_7 (22) = happyShift action_9
action_7 (4) = happyGoto action_12
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (18) = happyShift action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (18) = happyShift action_10
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (7) = happyShift action_26
action_10 (5) = happyGoto action_27
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (7) = happyShift action_26
action_11 (5) = happyGoto action_25
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (8) = happyShift action_14
action_12 (9) = happyShift action_15
action_12 (10) = happyShift action_16
action_12 (11) = happyShift action_17
action_12 (12) = happyShift action_18
action_12 (15) = happyShift action_24
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (8) = happyShift action_14
action_13 (9) = happyShift action_15
action_13 (10) = happyShift action_16
action_13 (11) = happyShift action_17
action_13 (12) = happyShift action_18
action_13 _ = happyReduce_9

action_14 (6) = happyShift action_4
action_14 (7) = happyShift action_2
action_14 (13) = happyShift action_5
action_14 (14) = happyShift action_6
action_14 (16) = happyShift action_7
action_14 (21) = happyShift action_8
action_14 (22) = happyShift action_9
action_14 (4) = happyGoto action_23
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (6) = happyShift action_4
action_15 (7) = happyShift action_2
action_15 (13) = happyShift action_5
action_15 (14) = happyShift action_6
action_15 (16) = happyShift action_7
action_15 (21) = happyShift action_8
action_15 (22) = happyShift action_9
action_15 (4) = happyGoto action_22
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (6) = happyShift action_4
action_16 (7) = happyShift action_2
action_16 (13) = happyShift action_5
action_16 (14) = happyShift action_6
action_16 (16) = happyShift action_7
action_16 (21) = happyShift action_8
action_16 (22) = happyShift action_9
action_16 (4) = happyGoto action_21
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (6) = happyShift action_4
action_17 (7) = happyShift action_2
action_17 (13) = happyShift action_5
action_17 (14) = happyShift action_6
action_17 (16) = happyShift action_7
action_17 (21) = happyShift action_8
action_17 (22) = happyShift action_9
action_17 (4) = happyGoto action_20
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (6) = happyShift action_4
action_18 (7) = happyShift action_2
action_18 (13) = happyShift action_5
action_18 (14) = happyShift action_6
action_18 (16) = happyShift action_7
action_18 (21) = happyShift action_8
action_18 (22) = happyShift action_9
action_18 (4) = happyGoto action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (8) = happyShift action_14
action_19 (9) = happyShift action_15
action_19 (10) = happyShift action_16
action_19 (11) = happyShift action_17
action_19 (12) = happyShift action_18
action_19 _ = happyReduce_7

action_20 (8) = happyShift action_14
action_20 (9) = happyShift action_15
action_20 (10) = happyShift action_16
action_20 (11) = happyShift action_17
action_20 (12) = happyShift action_18
action_20 _ = happyReduce_6

action_21 (8) = happyShift action_14
action_21 (9) = happyShift action_15
action_21 (10) = happyShift action_16
action_21 (11) = happyShift action_17
action_21 (12) = happyShift action_18
action_21 _ = happyReduce_5

action_22 (8) = happyShift action_14
action_22 (9) = happyShift action_15
action_22 (10) = happyShift action_16
action_22 (11) = happyShift action_17
action_22 (12) = happyShift action_18
action_22 _ = happyReduce_4

action_23 (8) = happyShift action_14
action_23 (9) = happyShift action_15
action_23 (10) = happyShift action_16
action_23 (11) = happyShift action_17
action_23 (12) = happyShift action_18
action_23 _ = happyReduce_8

action_24 _ = happyReduce_10

action_25 (17) = happyShift action_30
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (20) = happyShift action_29
action_26 _ = happyReduce_13

action_27 (17) = happyShift action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (19) = happyShift action_33
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (7) = happyShift action_26
action_29 (5) = happyGoto action_32
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (19) = happyShift action_31
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (6) = happyShift action_4
action_31 (7) = happyShift action_2
action_31 (13) = happyShift action_5
action_31 (14) = happyShift action_6
action_31 (16) = happyShift action_7
action_31 (21) = happyShift action_8
action_31 (22) = happyShift action_9
action_31 (4) = happyGoto action_35
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_14

action_33 (6) = happyShift action_4
action_33 (7) = happyShift action_2
action_33 (13) = happyShift action_5
action_33 (14) = happyShift action_6
action_33 (16) = happyShift action_7
action_33 (21) = happyShift action_8
action_33 (22) = happyShift action_9
action_33 (4) = happyGoto action_34
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (8) = happyShift action_14
action_34 (9) = happyShift action_15
action_34 (10) = happyShift action_16
action_34 (11) = happyShift action_17
action_34 (12) = happyShift action_18
action_34 _ = happyReduce_12

action_35 (8) = happyShift action_14
action_35 (9) = happyShift action_15
action_35 (10) = happyShift action_16
action_35 (11) = happyShift action_17
action_35 (12) = happyShift action_18
action_35 _ = happyReduce_11

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn4
		 (Tletter happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (Ttrue
	)

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (Tfalse
	)

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Tbinary Tand happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Tbinary Tor happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Tbinary Timp happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Tbinary Tequiv happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Tbinary Tequiv happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Tneg happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 6 4 happyReduction_11
happyReduction_11 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Tall happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 6 4 happyReduction_12
happyReduction_12 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Texist happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 (HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn5
		 ([Tvar happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn5
		 ((Tvar happy_var_1) : happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 23 23 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNeg -> cont 6;
	TokenWord happy_dollar_dollar -> cont 7;
	TokenBiop happy_dollar_dollar -> cont 8;
	TokenAnd -> cont 9;
	TokenOr -> cont 10;
	TokenImp -> cont 11;
	TokenEquiv -> cont 12;
	TokenTop -> cont 13;
	TokenBot -> cont 14;
	TokenRBracket -> cont 15;
	TokenLBracket -> cont 16;
	TokenRRBracket -> cont 17;
	TokenRLBracket -> cont 18;
	TokenCoron -> cont 19;
	TokenComma -> cont 20;
	TokenAll -> cont 21;
	TokenExists -> cont 22;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 23 tk tks = happyError' (tks, explist)
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

parseExpr :: String -> Either String Expr
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

