{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

{-|
Parsers for Juman dictionary files
-}

module Parser.Language.Japanese.Juman.ParseJumanDic (
  JumanWordContainer(..)
  , parseJumanDicList
  , parseJumanFile
  , dryRun
  ) where

import System.FilePath ((</>),takeBaseName) --filepath
import Control.Monad (forM,forM_)  --base
import qualified Data.Map as M     --base
import qualified Data.List as L    --base
import qualified System.IO as S    --base
import qualified Data.Text as T    --text
import qualified Data.Text.IO as T --text
import Text.Parsec                 --parsec
import Text.Parsec.Text            --parsec
import Parser.Language.Japanese.Juman.Config (Config(..),fetchConfig) --lightblue

-- | "JumanWordContainer" is a type for containing a juman word,
-- |  in the format:
-- |    JWC hinshi [(midashigo,score)] yomi map(=itemname,itemvalue) 
-- |    or JRengo word1 word2
-- |    or JComment comment (that follows ';' in a dictionary file)
data JumanWordContainer = 
  JWC T.Text [(T.Text,T.Text)] T.Text (M.Map T.Text T.Text)
  | JRengo JumanWordContainer JumanWordContainer
  | JComment T.Text
  | ParseError String T.Text
    deriving (Eq,Show)

-- | Return a parsed data structure of Juman dictionalies
parseJumanDicList :: IO [(FilePath,JumanWordContainer)]
parseJumanDicList = do
  config <- fetchConfig
  jwcs <- forM jumanDicList $ \relPath -> do
            S.hPutStrLn S.stderr $ "  Processing: " ++ relPath
            jwc <- parseJumanFile $ jumanDicDir config </> relPath
            let fileName = takeBaseName relPath
            return $ map (\j -> (fileName, j)) jwc
  return $ concat jwcs

-- | Relative paths of Juman dictionaries (that lightblue uses for the construction of Lexicon)
jumanDicList :: [FilePath] 
jumanDicList = [ 
    "basic/ContentW.dic",
    "basic/Noun.hukusi.dic", -- (名詞 (副詞的名詞 ((読み とおり)(見出し語 通り とおり)(意味情報 "代表表記:通り/とおり"))))
    "basic/Noun.koyuu.dic", -- (名詞 (人名 ((読み やまだ)(見出し語 山田)(意味情報 "人名:日本:姓:7:0.00607"))))
    "basic/Onomatopoeia.dic",
    "basic/Prefix.dic",
    "basic/Suffix.dic",
    "basic/Special.dic",
    --"basic/Noun.suusi.dic"
    --"basic/Postp.dic"   のエントリは MyLexicon.hs で別途定義
    --"basic/Assert.dic"  は不要
    --"basic/AuxV.dic"   のエントリは MyLexicon.hs で別途定義
    --"basic/Demonstrative.dic" のエントリは要対応
    "wik/Wikipedia.dic",
    "wik_new/wiktionary.dic",
    "wik_new/Wikipedia.dic",
    "automatic/Auto.dic",
    "automatic_new/Auto.dic",
    "automatic_new/shingojijiyougo.merge.dic"
    ]

-- | Test program for parseJumanDicList
dryRun :: IO()
dryRun = do
  putStrLn "\ndry-running parseJumanDicList:"
  jfc <- parseJumanDicList
  forM_ jfc $ \(_,j) -> case j of
                          ParseError string text -> (do; putStrLn string; T.putStrLn text)
                          _ -> return ()
  let (x,y,z,u) = L.foldl' (\(x,y,z,u) jfc -> case jfc of
                                                JWC _ _ _ _ -> (x+1,y,z,u)
                                                JRengo _ _ -> (x,y+1,z,u)
                                                JComment _ -> (x,y,z+1,u)
                                                ParseError _ _ -> (x,y,z,u+1)
                                                ) (0,0,0,0) $ snd $ unzip jfc
  putStrLn $ "\nJWC: " ++ (show x) ++ "\nJRengo: " ++ (show y)
    ++ "\nJComment: " ++ (show z) ++ "\nParseError: " ++ (show u)

-- | The function "parseJumanFile" parses a given Juman file.
parseJumanFile :: FilePath    -- ^ The path of the juman file (that is currently processed)
                  -> IO [JumanWordContainer]
parseJumanFile jumanFilePath = do  
  jumandic <- T.readFile jumanFilePath
  let jumanLines = filter (/= T.empty) $ T.lines jumandic --empty lines are ignored.
  return $ map parseJumanLine jumanLines

-- |
-- | A (Parsec) Parser for Juman Dictionary
-- | 
parseJumanLine :: T.Text -> JumanWordContainer
parseJumanLine jumanLine = 
  case parse jumanWordParser "" jumanLine of
    Left err -> ParseError (show err) jumanLine
    Right jwc -> jwc

wordParser :: Parser T.Text
wordParser = do
  word <- many1 letter
  return $ T.pack word  

yomiParser :: Parser T.Text
yomiParser = do
  string "(読み"
  space
  yomi <- mixedParser
  char ')'
  return yomi

midashigoListParser :: Parser [(T.Text,T.Text)]
midashigoListParser =
  try (do
    string "(見出し語"
    space
    midashigoList <- sepBy1 (midashigoWithScoreParser <|> midashigoParser) space
    char ')'
    return midashigoList
    )
  <|>
  try (do
    string "(見出し語"
    space
    midashigoList <- many1 midashigoWithScoreParser
    char ')'
    return midashigoList
    )

midashigoParser :: Parser (T.Text,T.Text)
midashigoParser =
  do
  midashigo <- mixedParser
  return $ (midashigo,(T.pack "10"))

midashigoWithScoreParser :: Parser (T.Text,T.Text)
midashigoWithScoreParser =
  do
  char '('
  midashigo <- mixedParser
  space
  d1 <- digit
  char '.'
  d2 <- digit
  char ')'
  --let ratio = toInteger (read [d1,d2]::Int) % 10
  return $ (midashigo,(T.pack [d1,d2]))

katsuyokeiParser :: Parser T.Text
katsuyokeiParser = do
  string "(活用型"
  space  
  katsuyokei <- wordParser
  char ')'
  return katsuyokei 

imijohoListParser :: Parser (M.Map T.Text T.Text)
imijohoListParser = do
  string "(意味情報"
  space
  char '\"'
  imijohoList <- sepBy1 (try imijohoParser <|> do{w <- mixedParser;return (w,T.empty)}) space
  string "\")"
  return $ M.fromList imijohoList 

mixedParser :: Parser T.Text
mixedParser = do
  word <- many1 (noneOf " \")")
  return $ T.pack word

imijohoParser :: Parser (T.Text,T.Text)
imijohoParser = do
  itemname <- wordParser
  char ':'
  item <- mixedParser 
  return (itemname,item) 

jumanWordParser :: Parser JumanWordContainer
jumanWordParser = do
  try ( do -- 任意スペース後に;が現れる行はコメント行
    spaces
    char ';'
    comment <- many anyChar
    return $ JComment (T.pack comment)
    )
  <|>
  -- (名詞 (普通名詞 ((読み あいきどう)(見出し語 合気道 (あいきどう 1.6))(意味情報 "代表表記:合気道/あいきどう カテゴリ:抽象物 ドメイン:スポーツ"))))
  try ( do 
    char '('
    pos <- wordParser
    space
    char '('
    subpos <- wordParser
    space
    char '('
    yomi <- yomiParser
    midashigoList <- midashigoListParser
    imimap <- imijohoListParser  
    string ")))"
    return $ JWC (T.intercalate ":" [pos,subpos]) midashigoList yomi imimap
    )
  <|>
  -- (動詞 ((読み あう)(見出し語 会う 逢う 遭う あう)(活用型 子音動詞ワ行)(意味情報 "代表表記:会う/あう 反義:動詞:分かれる/わかれる;動詞:別れる/わかれる")))
  -- (形容詞 ((読み あかい)(見出し語 赤い あかい)(活用型 イ形容詞アウオ段)(意味情報 "代表表記:赤い/あかい 名詞派生:赤/あか")))
  -- (形容詞 ((読み あいまいだ)(見出し語 曖昧だ (曖まいだ 1.6) (あい昧だ 1.6) (あいまいだ 1.6))(活用型 ナ形容詞)(意味情報 "代表表記:曖昧だ/あいまいだ")))
  try ( do 
    char '('
    pos <- wordParser
    space
    char '('
    yomi <- yomiParser
    midashigoList <- midashigoListParser
    katsuyokei <- katsuyokeiParser
    imimap <- imijohoListParser  
    string "))"
    return $ JWC (T.concat [pos,":",katsuyokei]) midashigoList yomi imimap
    )
  <|>
  try ( do  -- (活用型 ...)がないパターン
    char '('
    pos <- wordParser
    space
    char '('
    yomi <- yomiParser
    midashigoList <- midashigoListParser
    imimap <- imijohoListParser
    string "))"
    return $ JWC pos midashigoList yomi imimap
    )
  <|>
  -- (接尾辞 (形容詞性述語接尾辞 ((見出し語 (気だ 2.0)(げだ 2.0))(読み げだ)(活用型 ナノ形容詞)(意味情報 "代表表記:気だ/げだ 準内容語 カテゴリ:抽象物"))))
  try ( do --for Prefix.dic 接尾語
    char '('
    pos <- wordParser
    space
    char '('
    subpos <- wordParser
    space
    char '('
    midashigoList <- midashigoListParser
    optional space
    yomi <- yomiParser
    optional (try katsuyokeiParser)
    imimap <- imijohoListParser
    string ")))"
    return $ JWC (T.intercalate ":" [pos,subpos]) midashigoList yomi imimap
    )
  <|>
  -- (特殊 (句点 ((見出し語
  -- (特殊 (読点 ((見出し語
  -- (特殊 (括弧始
  -- (特殊 (括弧終
  try ( do -- for Special.dic 特殊
    char '('
    pos <- wordParser
    space
    char '('
    subpos <- wordParser
    space
    char '('
    midashigoList <- midashigoListParser
    yomi <- yomiParser
    string ")))"
    return $ JWC (T.intercalate ":" [pos,subpos]) midashigoList yomi M.empty
    )
  <|>
  -- (連語 ((名詞 (地名 ((読み しもきた)(見出し語 下北)(意味情報 "代表表記:下北/しもきた 地名:日本:青森県:郡 連語")))) (接尾辞 (名詞性特殊接尾辞 ((見出し語 郡)(読み ぐん)(意味情報 "代表表記:郡/ぐん 住所末尾 準内容語 カテゴリ:組織・団体:場所-その他 連語"))))))
  try ( do --for Noun.koyuu.dic 連語
    string "(連語 ("
    word1 <- jumanWordParser
    space
    word2 <- jumanWordParser
    string "))"
    return $ JRengo word1 word2
    )
