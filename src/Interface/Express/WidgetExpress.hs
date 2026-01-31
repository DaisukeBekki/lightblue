{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}

module Interface.Express.WidgetExpress (
    DisplaySetting(..)
  , LexicalItemsPosition(..)
  , defaultDisplaySetting
  , Widgetizable(..)
  ) where

import  Yesod
import  qualified Data.Maybe as Maybe --base
import  Interface.Text (SimpleText(..))
import  Interface.TeX (Typeset(..))
import  qualified Data.Text.Lazy as T
import  qualified Data.Text as StrictT
import  Parser.CCG (Node(..),RuleSymbol(..),Cat(..),isBaseCategory,Feature(..),FeatureValue(..))
import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.DTTwithName as DWN
import qualified DTS.UDTTdeBruijn as UDTT
import  qualified DTS.UDTTwithName as UDWN
import qualified DTS.QueryTypes as QT
import Interface.Tree
import qualified DTS.GeneralTypeQuery as GTQ


-- 表示設定レコード
data LexicalItemsPosition = LexTop | LexBottom | LexNone
  deriving (Eq, Show)

data DisplaySetting = DisplaySetting
  { defaultExpandDepth :: Int         -- ^ 何段までデフォルト展開するか
  , showCat :: Bool                   -- ^ 統語範疇を初期表示するか
  , showSem :: Bool                   -- ^ 意味表示を初期表示するか
  , leafVertical :: Bool              -- ^ 葉ノードを縦並び表示にするか
  , lexicalItemsPosition :: LexicalItemsPosition -- ^ Lexical Items の表示位置
  }

defaultDisplaySetting :: DisplaySetting
defaultDisplaySetting = DisplaySetting
  { defaultExpandDepth = 2
  , showCat = True
  , showSem = True
  , leafVertical = False
  , lexicalItemsPosition = LexTop
  }

-- Widgetizableクラスを定義、widgetizeという関数を持つ
class Widgetizable a where
  widgetize :: forall b. a -> WidgetT b IO ()
  widgetizeWith :: forall b. DisplaySetting -> a -> WidgetT b IO ()
  widgetizeWith _ = widgetize
  -- type Widget = WidgetT MyApp IO ()
  -- 任意の型のbにWidget（WidgetT 型）としてページに描画できるように変換する
  -- widgetize :: forall b. a -> Widgetと同じ
  
-- WidgetizableクラスのインスタンスにT.Text型を定義
instance Widgetizable T.Text where
  widgetize = toWidget 
  widgetizeWith _ = toWidget

-- WidgetizableクラスのインスタンスにNode型を定義
instance Widgetizable Node where
  widgetize = widgetizeWith defaultDisplaySetting
  widgetizeWith setting = widgetizeDepth setting 0

widgetizeDepth :: DisplaySetting -> Int -> Node -> WidgetT b IO ()
widgetizeDepth setting depth node = case Parser.CCG.daughters node of
  -- 子ノードなかったら
  -- widgetize node = case Parser.CCG.daughters node of
    [] -> do
      -- newIdent 関数で一意のidを得る
      id <- newIdent
      [whamlet|
        <table>
          <tr valign="bottom">
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="2">
                <!-- Nodeの音韻表示 -->
                <tr>
                  <td align="center" bgcolor="#002b5c" style="color: #ffffff;">#{pf node}
                <tr>
                  <td align="center">
                    <table border="0" cellpadding="0">
                      <tr class="cathide">
                        <td align="center">
                          <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetizeWith setting $ cat node}
                      <tr class="semhide">
                        <td>
                          <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetizeWith setting $ sem node}
            <td valign="baseline">
              <span>LEX
        |]
    dtrs -> do
      -- 子の数*２
      let len = (length dtrs)*2
      id <- newIdent
      let (styleA, classA, styleB, classB, buttonLabel) =
            if depth < defaultExpandDepth setting
              then ("display:block" :: StrictT.Text, "open" :: StrictT.Text, "display:none" :: StrictT.Text, "close" :: StrictT.Text, "-" :: StrictT.Text)
              else ("display:none" :: StrictT.Text, "close" :: StrictT.Text, "display:block" :: StrictT.Text, "open" :: StrictT.Text, "+" :: StrictT.Text)
      [whamlet|
         <table>
          <tr valign="bottom">
            <td valign="baseline">
              <div id=#{id}layerA style=#{styleA} class=#{classA}>
                <table border="1" rules="rows" frame="void" cellpadding="2">
                  <tr valign="bottom">
                    $forall dtr <- dtrs
                      <td align="center" valign="bottom">^{widgetizeDepth setting (depth + 1) dtr}&nbsp;
                  <tr>
                    <td align="center" colspan=#{len}>
                      <table border="0" cellpadding="0">
                        <tr class="cathide">
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetizeWith setting $ cat node}
                        <tr class="semhide">
                          <td>
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetizeWith setting $ sem node}
              <div id=#{id}layerB style=#{styleB} class=#{classB}>
                <table border="2" rules="rows" cellpadding="5" border="3px solid #808080">
                  <tr>
                    <td align="center" bgcolor="#002b5c" style="color: #ffffff;">^{widgetizeWith setting $ pf node}
                  <tr>
                    <td align="center" colspan=#{len}>
                      <table border="0" cellpadding="0">
                        <tr class="cathide">
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetizeWith setting $ cat node}
                        <tr class="semhide">
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetizeWith setting $ sem node}
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="5">
                <tr>
                  <td>
                   <button type="button" id=#{id}button onclick=toggle('#{id}')>#{buttonLabel}
                  <span>^{widgetizeWith setting $ rs node}
        |]

-- toText :: a -> Text
instance Widgetizable RuleSymbol where
  widgetize rs = [whamlet|#{toText rs}|]

instance Widgetizable Cat where
  widgetize category = case category of
    SL x y      -> [whamlet|<mrow>
                              ^{widgetize x}
                              <mo>/
                              ^{widgetize' y}
                              |]
    BS x y      -> [whamlet|<mrow>
                              ^{widgetize x}
                              <mo>\
                              ^{widgetize' y}
                              |]
    T True i _  -> [whamlet|<msub>
                              <mi>T
                              <mn>#{T.pack $ show i}
                              |]
    T False i u -> [whamlet|<msub>
                              ^{widgetize' u}
                              <mn>:[#{T.pack $ show i}]
                              |]
    S (pos:(conj:pm)) -> let x = toText pm
                             nullx = T.null x in
                         [whamlet|
                           <msub>
                             <mi>S
                             <mstyle color='Purple'>
                               <mtable columnalign='left'>
                                 <mtr class="sf">
                                   <mtd>^{widgetize pos}
                                 <mtr class="sf">
                                   <mtd>
                                     <mpadded height='-0.5em'>^{widgetize conj}
                                 <mtr class="sf">
                                   <mtd>
                                     <mpadded height='-0.5em'>^{widgetize pm}
                                 |]
    NP [cas]    -> [whamlet|<msub>
                              <mi>NP
                              <mtext class="sf">^{widgetize cas}
                           |]
    Sbar [sf]   -> [whamlet|<msub>
                              <menclose notation='top'>
                                <mi>S
                              <mtext class="sf">^{widgetize sf}
                           |]
    N           -> [whamlet|<mi>N|]
    CONJ        -> [whamlet|<mi>CONJ|]
    LPAREN      -> [whamlet|<mi>LPAREN|]
    RPAREN      -> [whamlet|<mi>RPAREN|]
    _           -> [whamlet|<mtext>Error: #{toText category}|]
    where widgetize' c = if isBaseCategory c 
                           then widgetize c
                           else [whamlet|<mrow>
                                           <mo>(
                                           ^{widgetize c}
                                           <mo>)
                                           |]

instance Widgetizable Feature where 
  widgetize (SF i f) = [whamlet|
                          <mtext>
                            #{toTeX f}
                          <mo>:
                          <mn>[#{T.pack (show i)}]
                          |]
  widgetize (F f) = [whamlet|<mtext>#{toTeX f}|]

instance Widgetizable [Feature] where
  widgetize pmfs = [whamlet|
                     #{T.intercalate "," $ Maybe.catMaybes $ pmfs2MathMLLoop ["t","p","n","N","T"] pmfs}
                     |]

pmfs2MathMLLoop :: [T.Text] -> [Feature] -> [Maybe T.Text]
pmfs2MathMLLoop labels pmfs = case (labels,pmfs) of
  ([],[])         -> []
  ((l:ls),(p:ps)) -> (pmf2MathML l p):(pmfs2MathMLLoop ls ps)
  _ -> [Just $ T.concat ["Error: mismatch in ", T.pack (show labels), " and ", T.pack (show pmfs)]]

pmf2MathML :: T.Text -> Feature -> Maybe T.Text
pmf2MathML label pmf = case (label,pmf) of
  (l,F [P])   -> Just $ T.concat ["+", l]
  (_,F [M])   -> Nothing -- if shared then Just $ T.concat ["{-}", l] else Nothing
  (l,F [P,M]) -> Just $ T.concat ["±", l]
  (l,F [M,P]) -> Just $ T.concat ["±", l]
  (l,SF i f)  -> do
                 x <- pmf2MathML l (F f)
                 return $ T.concat [x, ":[", T.pack $ show i, "]"]
  _ -> return $ T.pack "Error: pmf2MathML"

-- semのinstance化
-- fromDeBruijn :: [VarName] -> UDTTdB(=UDTT).Preterm -> UDWN.Preterm
instance Widgetizable UDTT.Preterm where
  widgetize = widgetize . (UDWN.fromDeBruijn [])

instance Widgetizable UDWN.VarName where
  widgetize (UDWN.VarName v i) =
    [whamlet|
      <msub>
        <mi>#{T.singleton v}
        <mn>#{T.pack (show i)}
        |]

-- UDWN: DTS.UDTTwithName
instance Widgetizable UDWN.Preterm where
  widgetize preterm = case preterm of
    UDWN.Var vname -> widgetize vname
    UDWN.Con cname -> [whamlet|<mtext>#{cname}|]
    UDWN.Type -> [whamlet|<mi>type|]
    UDWN.Kind -> [whamlet|<mi>kind|]
    UDWN.Pi vname a b -> [whamlet|
      <mrow>
        <mo>(
        ^{widgetize vname}
        <mo>:
        ^{widgetize a}
        <mo>)
        <mo>&rarr;
        ^{widgetize b}
        |]
    UDWN.Not a -> [whamlet|
      <mrow>
        <mo>&not;
        <mi>toMathML a
        |]
    UDWN.Lam vname m -> [whamlet|
      <mrow>
        <mi>&lambda;
        ^{widgetize vname}
        <mpadded lspace='-0.2em' width='-0.2em'>
          <mo>.
        ^{widgetize m}
        |]
    UDWN.App (UDWN.App (UDWN.Con cname) y) x -> [whamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{widgetize x}
        <mo>,
        ^{widgetize y}
        <mo>)
        |]
    UDWN.App (UDWN.App (UDWN.App (UDWN.Con cname) z) y) x -> [whamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{widgetize x}
        <mo>,
        ^{widgetize y}
        <mo>,
        ^{widgetize z}
        <mo>)
        |]
    UDWN.App (UDWN.App (UDWN.App (UDWN.App (UDWN.Con cname) u) z) y) x -> [whamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{widgetize x}
        <mo>,
        ^{widgetize y}
        <mo>,
        ^{widgetize z}
        <mo>,
        ^{widgetize u}
        <mo>)
        |]
    UDWN.App m n -> [whamlet|
      <mrow>
        ^{widgetize m}
        <mo>(
        ^{widgetize n}
        <mo>)
        |]
    UDWN.Sigma vname a b -> case b of 
      UDWN.Top -> widgetize a
      _   -> [whamlet|
        <mrow>
          <mo>[
          <mtable style="text-align: left;">
            <mtr style="text-align: left;">
              <mtd columnalign="left">
                <mrow>
                  ^{widgetize vname}
                  <mo>:
                  ^{widgetize a}
            <mtr style="text-align: left;">
              <mtd columnalign="left">
                <mpadded height='-0.5em'>^{widgetize b}
          <mo>]
        |]
    UDWN.Pair m n  -> [whamlet|
      <mrow>
        <mo>(
        ^{widgetize m}
        <mo>,
        ^{widgetize n}
        <mo>)
        |]
    UDWN.Proj s m  -> [whamlet|
      <mrow>
        <msub>
          <mi>&pi;
          <mi>#{toText s}
        <mo>(
        ^{widgetize m}
        <mo>)
        |]
    UDWN.Disj a b -> [whamlet|
      <mrow>
        <mi>
          ^{widgetize a}
        <mo>+
        <mi>
          ^{widgetize b}
        |]
    UDWN.Iota s p -> [whamlet|
        <mrow>
        <msub>
          <mi>&iota;
          <mi>#{toText s}
        <mo>(
        ^{widgetize p}
        <mo>)
        |]
    UDWN.Unpack p l m n -> [whamlet|
      <mrow>
        <msubsup>
          <mi>unpack
          <mn>^{widgetize l}
          <mn>^{widgetize p}
        <mo>(
        ^{widgetize m}
        <mo>,
        ^{widgetize n}
        <mo>)
        |]   
    UDWN.Lamvec vname m  -> [whamlet|
      <mrow>
        <mi>&lambda;
        <mover>
          ^{widgetize vname}
          <mo>&rarr;
        <mo>.
        ^{widgetize m}
        |]
    UDWN.Appvec vname m -> [whamlet|
      <mrow>
        ^{widgetize m}
        <mover>
          ^{widgetize vname}
          <mo>&rarr;
          |]
    UDWN.Unit       -> [whamlet|<mi>()|]
    UDWN.Top        -> [whamlet|<mi>&top;|]
    UDWN.Entity     -> [whamlet|<mi>entity|]
    UDWN.Bot        -> [whamlet|<mi>&bot;|]
    UDWN.Asp m    -> [whamlet|
      <mrow>
        <mo>@
        ^{widgetize m}
      |]
    UDWN.Ann m a   -> [whamlet|
      <mrow>
        <mo>(
        ^{widgetize m}
        <mo>::
        ^{widgetize a}
        <mo>)
      |]
    UDWN.Nat    -> [whamlet|<mi>N|]
    UDWN.Zero   -> [whamlet|<mi>0|]
    UDWN.Succ n -> [whamlet|
      <mrow>
        <mi>s
        ^{widgetize n}
        |]
    UDWN.Natrec p n e f -> [whamlet|
      <mrow>
        <msubsup>
          <mi>natrec
          <mn>^{widgetize p}
          <mn>^{widgetize n}
        <mo>(
        ^{widgetize e}
        <mo>,
        ^{widgetize f}
        <mo>)
        |]
    UDWN.Eq a m n -> [whamlet|
      <mrow>
        ^{widgetize m}
        <msub>
          <mo>=
          ^{widgetize a}
        ^{widgetize n}
        |]
    UDWN.Refl a m -> [whamlet|
      <mrow>
        <mi>refl
        ^{widgetize a}
        <mo>(
        ^{widgetize m}
        <mo>)
        |]
    UDWN.Idpeel p e r -> [whamlet|
      <mrow>
        <msubsup>
          <mi>idpeel
          <mn>^{widgetize p}
          <mn>^{widgetize e}
        <mo>(
        ^{widgetize r}
        <mo>)
        |]

-- fromDeBruijn :: [VarName] -> DTTdB(=DTT).Preterm -> DWN.Preterm
instance Widgetizable DTT.Preterm where
  widgetize = widgetize . (DWN.fromDeBruijn [])

instance Widgetizable DWN.VarName where
  widgetize (DWN.VarName v i) =
    [whamlet|
      <msub>
        <mi>#{T.singleton v}
        <mn>#{T.pack (show i)}
        |]


instance Widgetizable DWN.Preterm where
  widgetize preterm = case preterm of
    DWN.Var vname -> widgetize vname
    DWN.Con cname -> [whamlet|<mtext>#{cname}|]
    DWN.Type -> [whamlet|<mi>type|]
    DWN.Kind -> [whamlet|<mi>kind|]
    DWN.Pi vname a b -> [whamlet|
      <mrow>
        <mo>(
        ^{widgetize vname}
        <mo>:
        ^{widgetize a}
        <mo>)
        <mo>&rarr;
        ^{widgetize b}
        |]
    DWN.Not a -> [whamlet|
      <mrow>
        <mi>&not;
        <mi>toMathML a
        |]
    DWN.Sigma vname a b -> case b of 
      DWN.Top -> widgetize a
      _   -> [whamlet|
        <mrow>
          <mo>[
          <mtable style="text-align: left;">
            <mtr style="text-align: left";>
              <mtd columnalign="left">
                <mrow>
                  ^{widgetize vname}
                  <mo>:
                  ^{widgetize a}
            <mtr style="text-align: left";>
              <mtd columnalign="left">
                <mpadded height='-0.5em'>^{widgetize b}
          <mo>]
        |]
    DWN.Lam vname m -> [whamlet|
      <mrow>
        <mi>&lambda;
        ^{widgetize vname}
        <mpadded lspace='-0.2em' width='-0.2em'>
          <mo>.
        ^{widgetize m}
        |]
    DWN.App (DWN.App (DWN.Con cname) y) x -> [whamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{widgetize x}
        <mo>,
        ^{widgetize y}
        <mo>)
        |]
    DWN.App (DWN.App (DWN.App (DWN.Con cname) z) y) x -> [whamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{widgetize x}
        <mo>,
        ^{widgetize y}
        <mo>,
        ^{widgetize z}
        <mo>)
        |]
    DWN.App (DWN.App (DWN.App (DWN.App (DWN.Con cname) u) z) y) x -> [whamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{widgetize x}
        <mo>,
        ^{widgetize y}
        <mo>,
        ^{widgetize z}
        <mo>,
        ^{widgetize u}
        <mo>)
        |]
    DWN.App m n -> [whamlet|
      <mrow>
        ^{widgetize m}
        <mo>(
        ^{widgetize n}
        <mo>)
        |]
    DWN.Pair m n  -> [whamlet|
      <mrow>
        <mo>(
        ^{widgetize m}
        <mo>,
        ^{widgetize n}
        <mo>)
        |]
    DWN.Proj s m  -> [whamlet|
      <mrow>
        <msub>
          <mi>&pi;
          <mi>#{toText s}
        <mo>(
        ^{widgetize m}
        <mo>)
        |]
    DWN.Disj a b -> [whamlet|
      <mrow>
        <mi>
          ^{widgetize a}
        <mo>+
        <mi>
          ^{widgetize b}
        |]
    DWN.Iota s p -> [whamlet|
        <mrow>
        <msub>
          <mi>&iota;
          <mi>#{toText s}
        <mo>(
        ^{widgetize p}
        <mo>)
        |]
    DWN.Unpack p l m n -> [whamlet|
      <mrow>
        <msubsup>
          <mi>unpack
          <mn>^{widgetize l}
          <mn>^{widgetize p}
        <mo>(
        ^{widgetize m}
        <mo>,
        ^{widgetize n}
        <mo>)
        |] 
    DWN.Unit       -> [whamlet|<mi>()|]
    DWN.Top        -> [whamlet|<mi>&top;|]
    DWN.Entity     -> [whamlet|<mi>entity|]
    DWN.Bot        -> [whamlet|<mi>&bot;|]
    DWN.Nat    -> [whamlet|<mi>N|]
    DWN.Zero   -> [whamlet|<mi>0|]
    DWN.Succ n -> [whamlet|
      <mrow>
        <mi>s
        ^{widgetize n}
        |]
    DWN.Natrec p n e f -> [whamlet|
      <mrow>
        <msubsup>
          <mi>natrec
          <mn>^{widgetize p}
          <mn>^{widgetize n}
        <mo>(
        ^{widgetize e}
        ^{widgetize f}
        <mo>)
        |]
    DWN.Eq a m n -> [whamlet|
      <mrow>
        ^{widgetize m}
        <msub>
          <mo>=
          ^{widgetize a}
        ^{widgetize n}
        |]
    DWN.Refl a m -> [whamlet|
      <mrow>
        <mi>refl
        ^{widgetize a}
        <mo>(
        ^{widgetize m}
        <mo>)
        |]
    DWN.Idpeel p e r -> [whamlet|
      <mrow>
        <msubsup>
          <mi>idpeel
          <mn>^{widgetize p}
          <mn>^{widgetize e}
        <mo>(
        ^{widgetize r}
        <mo>)
        |]

instance Widgetizable QT.DTTrule where
  widgetize rule = case rule of
    QT.Var -> [whamlet| <mi>#{toText QT.Var}|]
    QT.Con -> [whamlet| <mi>#{toText QT.Con}|]
    QT.TypeF -> [whamlet| <mi>#{toText QT.TypeF}|]
    QT.Conv -> [whamlet| <mi>#{toText QT.Conv}|]
    QT.WK -> [whamlet| <mi>#{toText QT.WK}|]
    QT.PiF -> [whamlet| <mi>ΠF|]
    QT.PiI -> [whamlet| <mi>ΠI|]
    QT.PiE -> [whamlet| <mi>ΠE|]
    QT.SigmaF -> [whamlet| <mi>ΣF|]
    QT.SigmaI -> [whamlet| <mi>ΣI|]
    QT.SigmaE ->[whamlet| <mi>ΣE|]
    QT.DisjF -> [whamlet| <mi>+F|]
    QT.DisjI -> [whamlet| <mi>+I|]
    QT.DisjE -> [whamlet| <mi>+E|]
    QT.BotF -> [whamlet| <mi>⊥F|]
    QT.TopF -> [whamlet| <mi>TF|]
    QT.TopI -> [whamlet| <mi>TI|]
    QT.EnumF ->[whamlet| <mi>{}F|]
    QT.EnumI -> [whamlet| <mi>{}I|]
    QT.EnumE -> [whamlet| <mi>{}E|]
    QT.IqF -> [whamlet| <mi>=F|]
    QT.IqI ->[whamlet| <mi>=I|]
    QT.IqE -> [whamlet| <mi>=E|]
    QT.NatF ->[whamlet| <mi>NatF|]
    QT.NatI ->[whamlet| <mi>NatI|]
    QT.NatE ->[whamlet| <mi>NatE|]

-- type Signature = [(T.Text, Preterm)]
instance Widgetizable DTT.Signature where
  widgetize signature = 
    let reversedItems = reverse signature
        itemsWithFlags = case reversedItems of
          [] -> []
          _  -> zip reversedItems (repeat False) ++ [(last reversedItems, True)]
    in [whamlet|
       <mrow>
         $forall ((nm, tm), isLast) <- itemsWithFlags
           <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize nm}
           <mo>:
           <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize tm}
           $if not isLast
             <mo>,
     |]

-- type Signature = [(T.Text, Preterm)]
instance Widgetizable DWN.Signature where
  widgetize signature = 
    let reversedItems = reverse signature
        itemsWithFlags = case reversedItems of
          [] -> []
          _  -> zip reversedItems (repeat False) ++ [(last reversedItems, True)]
    in [whamlet|
       <math xmlns="http://www.w3.org/1998/Math/MathML">
       <mrow>
         $forall ((nm, tm), isLast) <- itemsWithFlags
           <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize nm}
           <mo>:
           <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize tm}
           $if not isLast
             <mo>,
     |]

-- type Context = [(VarName, Preterm)]
instance Widgetizable DWN.Context where
  widgetize context = 
    let reversedItems = reverse context
        itemsWithFlags = case reversedItems of
          [] -> []
          _  -> zip (init reversedItems) (repeat False) ++ [(last reversedItems, True)]
    in [whamlet|
         <math xmlns="http://www.w3.org/1998/Math/MathML">
           <mrow>
             $forall ((nm, tm), isLast) <- itemsWithFlags
               <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize nm}
               <mo>:
               <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize tm}
               $if not isLast
                 <mo>,
      |]


-- type Context = [Preterm]
instance Widgetizable DTT.Context where
  widgetize context = 
    let reversedItems = reverse context
        itemsWithFlags = case reversedItems of
          [] -> []
          _ -> zip reversedItems (repeat False) ++ [(last reversedItems, True)]
    in [whamlet|
         <math xmlns="http://www.w3.org/1998/Math/MathML">
           <mrow>
             $forall (cotx, isLast) <- itemsWithFlags
               <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize cotx}
               $if not isLast
                 <mo>,
       |]

-- 
instance Widgetizable DTT.Judgment where 
  widgetize (DTT.Judgment sig cxt trm typ) =
    [whamlet|
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <mrow>
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize cxt}
          <mo>&vdash;
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize trm}
          <mo>:
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize typ}
    |]

instance Widgetizable UDWN.Judgment where
  widgetize (UDWN.Judgment sig cxt trm typ) = 
    [whamlet|
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <mrow>
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize cxt}
          <mo>&vdash;
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize trm}
          <mo>:
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize typ}
    |]

instance Widgetizable DWN.Judgment where
  widgetize (DWN.Judgment sig cxt trm typ) = 
    [whamlet|
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <mrow>
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize cxt}
          <mo>&vdash;
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize trm}
          <mo>:
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize typ}
    |]

-- node :: DTT.Judgment
-- DWN.fromDeBruijnJudgment :: DTTdB.Judgment -> DWN.Judgment
instance (Widgetizable r, a ~ DTT.Judgment) => Widgetizable (Tree r a) where
  widgetize (Tree rule node dtrs) = case dtrs of
    [] -> do
      id <- newIdent
      [whamlet|
        <math xmlns="http://www.w3.org/1998/Math/MathML">
          <mstyle displaystyle="true">
            <mfrac linethickness="medium">
              <mrow>
              <mrow>^{widgetize $ DWN.fromDeBruijnJudgment node}
          <mstyle fontsize="0.8" color="Black">
              <mo>(</mo>
              <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize rule}
              <mo>)</mo>
      |]
    dtrs -> do
      let len = (length dtrs)*5
      id <- newIdent
      [whamlet|
        <math xmlns="http://www.w3.org/1998/Math/MathML">
          <table>
            <tr valign="bottom">
              <td valign="baseline">
                <div id=#{StrictT.concat [id, "layerA"]} style="display: none" class="close">
                  <table border="1" rules="rows" frame="void" cellpadding="2">
                    <tr valign="bottom">
                      $forall dtr <- dtrs
                        <td align="center" valign="bottom">^{widgetize dtr}&nbsp;
                    <tr>
                      <td align="center" colspan=#{len}>
                        <table border="0" cellpadding="0">
                          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize $ DWN.fromDeBruijnJudgment node}
                <div id=#{StrictT.concat [id, "layerB"]} style="display: block" border="2px solid #000" class="open">
                  <table border="20" rules="rows" cellpadding="5" border="3px solid #808080">
                    <tr align="center" colspan=#{len}>
                      <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize $ DWN.fromDeBruijnJudgment node}
              <td valign="baseline">
                <table border="1" rules="rows" frame="void" cellpadding="5">
                  <tr>
                    <td>
                      <mo>(</mo>
                      <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize rule}
                      <mo>)</mo>
                    <td>
                      <button type="button" class="btn-design" id=#{StrictT.concat [id, "button"]} onclick=toggle('#{id}')>+
      |]


instance Widgetizable UDTT.Judgment where 
  widgetize (UDTT.Judgment sig cxt trm typ) = 
    [whamlet|
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <mrow>
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize cxt}
          <mo>&vdash;
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize trm}
          <mo>:
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize typ}
    |]

-- Proof search query: Γ ⊢ ? : A
instance Widgetizable DTT.ProofSearchQuery where
  widgetize = widgetize . DTT.embedProofSearchQuery

-- GeneralTypeQuery (generic judgment-like view)
instance (Widgetizable b, Widgetizable (GTQ.QueryGoal c), Widgetizable (GTQ.QueryGoal d)) => Widgetizable (GTQ.GeneralTypeQuery a b c d) where
  widgetize (GTQ.GeneralTypeQuery _ cxt trm typ) =
    [whamlet|
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <mrow>
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize cxt}
          <mo>&vdash;
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize trm}
          <mo>:
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{widgetize typ}
    |]
  
instance (Widgetizable a) => Widgetizable (GTQ.QueryGoal a) where
  widgetize (GTQ.Term t) = widgetize t
  widgetize GTQ.Question = [whamlet|<mo>?</mo>|]

instance Widgetizable DWN.ProofSearchQuery where
  widgetize = widgetize . DWN.embedProofSearchQuery