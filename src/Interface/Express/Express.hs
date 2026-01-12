{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Interface.Express.Express (
    showExpress
  , showExpressInference
  , setDisplaySetting
  , setDisplayOptions
  , setPrewarmOptions
  ) where

import Yesod
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text as TS          -- strict text for JSON
import qualified Interface.Express.Lightblue as L
import Data.List (null, find, zip7, maximumBy, sortOn, nubBy)
import qualified Interface.Express.WidgetExpress as WE
import qualified DTS.NaturalLanguageInference as NLI
import Text.Julius (juliusFile)
import  Text.Cassius (cassiusFile)
import System.Process (callCommand)
import Control.Concurrent (forkIO, threadDelay)
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, IOException, try, SomeException)
import System.Info (os)
import Data.IORef(IORef, readIORef, atomicWriteIORef, newIORef, atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)
import qualified Parser.CCG as CCG (Node, showScore, getLeafNodesFromNode, pf, cat, sem, sig, score)
import qualified Parser.ChartParser as CP
import qualified Parser.LangOptions as PL (defaultJpOptions)
import qualified Parser.Language.Japanese.Lexicon as JP (setupLexicon)
import qualified Parser.PartialParsing as Partial
import qualified Data.Map as M
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified Data.Store as Store
import qualified Data.ByteString as BS
import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.DTTwithName as DWN
import qualified DTS.UDTTdeBruijn as UDTT
import qualified DTS.TypeChecker as TY
import qualified Interface.Tree as Tree
import Interface.Text (SimpleText(..))
import Data.Char (toLower)
import qualified ListT as LT (ListT, uncons, toList, take)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.Store as Store

-- JSeM 用: 各文の N-best ノードを保持する IORef
-- [(入力文, その文に対する [CCG.Node])] を格納
-- 進捗管理用
data SentenceProgress = SentenceProgress
  { snText  :: T.Text
  , snNodes :: [CCG.Node]
  , snDone  :: Bool
  }

-- 選択された TypeCheckDiagram（文index -> diagram）
data TCSelection = TCSelection { selNodeIdx :: Int, selDiagIdx :: Int, selDiagram :: QT.DTTProofDiagram }

data TCStatus = TCNotStarted | TCInProgress | TCDone [QT.DTTProofDiagram] | TCFailed TS.Text

-- アプリケーションの状態として ParseResult を保持するための IORef を定義
{-# NOINLINE currentParseResultRef #-}
currentParseResultRef :: IORef (Maybe NLI.ParseResult)
currentParseResultRef = unsafePerformIO $ newIORef Nothing

{-# NOINLINE currentDiscourseNodesRef #-}
currentDiscourseNodesRef :: IORef (Maybe [SentenceProgress])
currentDiscourseNodesRef = unsafePerformIO $ newIORef Nothing

-- TypeCheck 設定・状態
{-# NOINLINE currentProverRef #-}
currentProverRef :: IORef (Maybe QT.Prover)
currentProverRef = unsafePerformIO $ newIORef Nothing

{-# NOINLINE currentBaseSignatureRef #-}
currentBaseSignatureRef :: IORef DTT.Signature
currentBaseSignatureRef = unsafePerformIO $ newIORef []

{-# NOINLINE currentBaseContextRef #-}
currentBaseContextRef :: IORef DTT.Context
currentBaseContextRef = unsafePerformIO $ newIORef []

{-# NOINLINE currentNTypeCheckRef #-}
currentNTypeCheckRef :: IORef Int
currentNTypeCheckRef = unsafePerformIO $ newIORef 1

{-# NOINLINE currentNProofRef #-}
currentNProofRef :: IORef Int
currentNProofRef = unsafePerformIO $ newIORef 1

{-# NOINLINE currentVerboseRef #-}
currentVerboseRef :: IORef Bool
currentVerboseRef = unsafePerformIO $ newIORef False

{-# NOINLINE currentTCStateRef #-}
currentTCStateRef :: IORef (M.Map (Int,Int) TCStatus)
currentTCStateRef = unsafePerformIO $ newIORef M.empty

{-# NOINLINE currentTCSelectionRef #-}
currentTCSelectionRef :: IORef (M.Map Int TCSelection)
currentTCSelectionRef = unsafePerformIO $ newIORef M.empty

-- Proof search state
{-# NOINLINE currentPSQPosRef #-}
currentPSQPosRef :: IORef (Maybe DTT.ProofSearchQuery)
currentPSQPosRef = unsafePerformIO $ newIORef Nothing

{-# NOINLINE currentPSQNegRef #-}
currentPSQNegRef :: IORef (Maybe DTT.ProofSearchQuery)
currentPSQNegRef = unsafePerformIO $ newIORef Nothing

{-# NOINLINE currentPSPosRef #-}
currentPSPosRef :: IORef [QT.DTTProofDiagram]
currentPSPosRef = unsafePerformIO $ newIORef []

{-# NOINLINE currentPSNegRef #-}
currentPSNegRef :: IORef [QT.DTTProofDiagram]
currentPSNegRef = unsafePerformIO $ newIORef []

-- ProofSearch cache (for prewarm/lookup)
data PSCacheEntry = PSCacheEntry
  { cachePos     :: [QT.DTTProofDiagram]
  , cacheNeg     :: [QT.DTTProofDiagram]
  , cachePosDone :: Bool
  , cacheNegDone :: Bool
  , cacheCtxHashes :: [BS.ByteString]
  , cacheExtraHashes :: [BS.ByteString]
  }

emptyCacheEntry :: PSCacheEntry
emptyCacheEntry = PSCacheEntry [] [] False False [] []

{-# NOINLINE proofCacheRef #-}
proofCacheRef :: IORef (M.Map BS.ByteString PSCacheEntry)
proofCacheRef = unsafePerformIO $ newIORef M.empty

-- limit concurrent proofsearch tasks
{-# NOINLINE activeProofThreadsRef #-}
activeProofThreadsRef :: IORef Int
activeProofThreadsRef = unsafePerformIO $ newIORef 0

-- global halt: stop broad prewarm once any proof is found
{-# NOINLINE prewarmHaltRef #-}
prewarmHaltRef :: IORef Bool
prewarmHaltRef = unsafePerformIO $ newIORef False

-- configurable: prewarm top-k and concurrency
{-# NOINLINE currentPrewarmTopKRef #-}
currentPrewarmTopKRef :: IORef Int
currentPrewarmTopKRef = unsafePerformIO $ newIORef 3

{-# NOINLINE currentPrewarmParallelRef #-}
currentPrewarmParallelRef :: IORef Int
currentPrewarmParallelRef = unsafePerformIO $ newIORef 2

setPrewarmOptions :: Int -> Int -> IO ()
setPrewarmOptions k par = do
  atomicWriteIORef currentPrewarmTopKRef (max 1 k)
  atomicWriteIORef currentPrewarmParallelRef (max 1 par)

-- schedule with concurrency limit
scheduleLimited :: IO () -> IO ()
scheduleLimited action = do
  let waitLoop = do
        lim <- readIORef currentPrewarmParallelRef
        cur <- readIORef activeProofThreadsRef
        if cur >= lim then threadDelay 200000 >> waitLoop else return ()
  waitLoop
  atomicModifyIORef' activeProofThreadsRef (\x -> (x+1, ()))
  _ <- forkIO $ do
    catch action (const (return ()) :: SomeException -> IO ())
    atomicModifyIORef' activeProofThreadsRef (\x -> (max 0 (x-1), ()))
    return ()
  return ()

-- Ensure a PSQ is being cached; if absent, start streaming results into cache
ensureCachedWithTerms :: DTT.ProofSearchQuery -> [DTT.Preterm] -> Int -> QT.Prover -> IO ()
ensureCachedWithTerms psq extraTerms nProof prover = do
  let key = Store.encode psq
      isNegQ = case psq of
                 DTT.ProofSearchQuery _ _ t -> case t of
                   DTT.Pi _ DTT.Bot -> True
                   _                -> False
  mp <- readIORef proofCacheRef
  halted <- readIORef prewarmHaltRef
  when halted (return ())
  when (M.notMember key mp) $ do
    let ctxHashes = case psq of
                      DTT.ProofSearchQuery _ ctx _ -> map Store.encode ctx
        extraHashes = map Store.encode extraTerms
    atomicModifyIORef' proofCacheRef (\m0 ->
      let base = emptyCacheEntry { cacheCtxHashes = ctxHashes, cacheExtraHashes = extraHashes }
      in (M.insertWith (\_ old -> old) key base m0, ()))
    -- run POS/NEG depending on query's goal (we don't know here; run once)
    _ <- scheduleLimited $ do
      let loop l updDone updList = do
            m <- LT.uncons l
            case m of
              Nothing -> atomicModifyIORef' proofCacheRef (\m0 ->
                           let ce = M.findWithDefault emptyCacheEntry key m0
                               ce' = updDone ce
                           in (M.insert key ce' m0, ()))
              Just (d, rest) -> do
                atomicModifyIORef' proofCacheRef (\m0 ->
                  let ce = M.findWithDefault emptyCacheEntry key m0
                      ce' = updList ce d
                  in (M.insert key ce' m0, ()))
            -- stop after first found (one is enough)
            atomicWriteIORef prewarmHaltRef True
            atomicModifyIORef' proofCacheRef (\m0 ->
              let ce = M.findWithDefault emptyCacheEntry key m0
                  ce' = updDone ce
              in (M.insert key ce' m0, ()))
            return ()
      if isNegQ
        then loop (LT.take nProof (prover psq))
                  (\e -> e { cacheNegDone = True })
                  (\e d -> e { cacheNeg = cacheNeg e ++ [d] })
        else loop (LT.take nProof (prover psq))
                  (\e -> e { cachePosDone = True })
                  (\e d -> e { cachePos = cachePos e ++ [d] })
    return ()

ensureCached :: DTT.ProofSearchQuery -> Int -> QT.Prover -> IO ()
ensureCached psq nProof prover = ensureCachedWithTerms psq [] nProof prover

-- Only ensure cache entry exists and record extra term hashes (no proving)
addTermsToCache :: DTT.ProofSearchQuery -> [DTT.Preterm] -> IO ()
addTermsToCache psq extra = do
  let key = Store.encode psq
      ctxHashes = case psq of
                    DTT.ProofSearchQuery _ ctx _ -> map Store.encode ctx
      extraHashes = map Store.encode extra
  atomicModifyIORef' proofCacheRef (\m0 ->
    let base = emptyCacheEntry { cacheCtxHashes = ctxHashes, cacheExtraHashes = extraHashes }
        upd e = e { cacheCtxHashes = cacheCtxHashes e ++ ctxHashes
                  , cacheExtraHashes = cacheExtraHashes e ++ extraHashes
                  }
    in (M.insertWith (\new old -> upd old) key base m0, ()))
{-# NOINLINE currentPSDonePosRef #-}
currentPSDonePosRef :: IORef Bool
currentPSDonePosRef = unsafePerformIO $ newIORef False

{-# NOINLINE currentPSDoneNegRef #-}
currentPSDoneNegRef :: IORef Bool
currentPSDoneNegRef = unsafePerformIO $ newIORef False

-- 表示設定を保持する IORef
{-# NOINLINE currentDisplaySettingRef #-}
currentDisplaySettingRef :: IORef WE.DisplaySetting
currentDisplaySettingRef = unsafePerformIO $ newIORef WE.defaultDisplaySetting

setDisplaySetting :: WE.DisplaySetting -> IO ()
setDisplaySetting dsp = atomicWriteIORef currentDisplaySettingRef dsp

-- | Helper: set by CLI options without importing WidgetExpress from caller
setDisplayOptions :: Maybe Int -> Bool -> Bool -> Bool -> IO ()
setDisplayOptions mDepth noShowCat noShowSem leafVert = do
  let base = WE.defaultDisplaySetting
      base' = maybe base (\d -> base { WE.defaultExpandDepth = d }) mDepth
      base'' = if noShowCat then base' { WE.showCat = False } else base'
      base''' = if noShowSem then base'' { WE.showSem = False } else base''
      dsp = base''' { WE.leafVertical = leafVert }
  setDisplaySetting dsp

data App = App

-- normalize signature for stable PSQ keys
normalizeSignature :: DTT.Signature -> DTT.Signature
normalizeSignature =
  nubBy (\(a,_) (b,_) -> a == b) . sortOn fst


mkYesod "App" [parseRoutes|
/parsing ParsingR GET
/inference InferenceR GET
/inference/progress InfProgressR GET
/inference/col InfColR GET
/inference/typecheck InfTypecheckStartR GET
/inference/typecheck/result InfTypecheckResultR GET
/inference/typecheck/select InfTypecheckSelectR GET
/proofsearch ProofSearchR GET
/proofsearch/progress ProofProgressR GET
/proofsearch/list ProofListR GET
/proofsearch/query ProofQueryR GET
/proofsearch/query/bin ProofQueryBinR GET
/proofsearch/start ProofStartR GET
/span SpanR GET
/span/node NodeR GET
/proofcache/progress ProofCacheProgressR GET
/proofcache/marks ProofCacheMarksR GET
/export/sem ExportSemR GET
/export/sem/text ExportSemTextR GET
/export/node ExportNodeR GET
/error ErrorR GET
|]

instance Yesod App

showExpressInference :: CP.ParseSetting -> QT.Prover -> DTT.Signature -> DTT.Context -> [T.Text] -> IO ()
showExpressInference ps _prover _signtr _contxt sentences = do
  -- 環境変数から表示設定を読み込み、反映
  applyEnvDisplayOptions

  -- TypeCheck 設定を保持
  atomicWriteIORef currentProverRef (Just _prover)
  atomicWriteIORef currentBaseSignatureRef _signtr
  atomicWriteIORef currentBaseContextRef _contxt
  atomicWriteIORef currentNTypeCheckRef (CP.nTypeCheck ps)
  atomicWriteIORef currentVerboseRef (CP.verbose ps)
  atomicWriteIORef currentNProofRef (CP.nProof ps)
  -- Prewarm options (env override)
  mTopK <- lookupEnv "LB_PREWARM_TOPK"
  mPar  <- lookupEnv "LB_PREWARM_PARALLEL"
  case (mTopK >>= readMaybe, mPar >>= readMaybe) of
    (Just k, Just p) -> setPrewarmOptions k p
    (Just k, Nothing) -> do
      p <- readIORef currentPrewarmParallelRef
      setPrewarmOptions k p
    (Nothing, Just p) -> do
      k <- readIORef currentPrewarmTopKRef
      setPrewarmOptions k p
    _ -> return ()

  -- 逐次パース
  let discourseLT = NLI.sequentialParsing ps sentences
  let idxd = zip ([0..] :: [Int]) discourseLT
      initProgress = [ SentenceProgress txt [] False
                     | (_i,(txt,_)) <- idxd
                     ]
  atomicWriteIORef currentDiscourseNodesRef (Just initProgress)
  -- ローカル関数群
  let updateAt :: Int -> (SentenceProgress -> SentenceProgress) -> [SentenceProgress] -> [SentenceProgress]
      updateAt _ _ [] = []
      updateAt 0 f (x:xs) = f x : xs
      updateAt n f (x:xs) = x : updateAt (n-1) f xs
      appendNode :: Int -> CCG.Node -> IO ()
      appendNode idx node =
        atomicModifyIORef' currentDiscourseNodesRef $ \m ->
          case m of
            Nothing   -> (m, ())
            Just list -> (Just (updateAt idx (\sp -> sp { snNodes = snNodes sp ++ [node] }) list), ())
      markDone :: Int -> IO ()
      markDone idx =
        atomicModifyIORef' currentDiscourseNodesRef $ \m ->
          case m of
            Nothing   -> (m, ())
            Just list -> (Just (updateAt idx (\sp -> sp { snDone = True }) list), ())
      consume :: Int -> LT.ListT IO CCG.Node -> IO ()
      consume idx lst = do
        m <- LT.uncons lst
        case m of
          Nothing -> markDone idx
          Just (node, rest) -> do
            appendNode idx node
            consume idx rest
  -- 各文ごとにバックグラウンドで ListT を uncons して snNodes を追加
  mapM_ (\(idx, (_txt, lst)) -> do
            _ <- forkIO $ consume idx lst
            return ()
        ) (zip ([0..] :: [Int]) discourseLT)

  -- 事前探索
  let prewarmBest :: IO ()
      prewarmBest = do
        let waitLoop = do
              md <- readIORef currentDiscourseNodesRef
              case md of
                Nothing -> threadDelay 300000 >> waitLoop
                Just disc ->
                  if all snDone disc
                    then do
                      mProver <- readIORef currentProverRef
                      baseSig <- readIORef currentBaseSignatureRef
                      baseCtx <- readIORef currentBaseContextRef
                      nProof  <- readIORef currentNProofRef
                      nType   <- readIORef currentNTypeCheckRef
                      verbose <- readIORef currentVerboseRef
                      case mProver of
                        Nothing -> return ()
                        Just prover -> do
                          -- 各文の top-1 ノードを選択（score 降順）
                          let pickBest sp =
                                case reverse (snNodes sp) of
                                  [] -> Nothing
                                  xs -> Just $ maximumBy (\a b -> compare (CCG.score a) (CCG.score b)) xs
                              bests = map pickBest disc
                          if any (== Nothing) bests
                            then return ()
                            else do
                              -- 逐次 TypeCheck で context と signature を構築
                              let nodes = [ n | Just n <- bests ]
                              let build :: DTT.Signature -> DTT.Context -> [DTT.Preterm] -> [CCG.Node] -> IO (Maybe (DTT.Signature, DTT.Context, [DTT.Preterm]))
                                  build sigAcc ctxAcc used [] = return (Just (sigAcc, ctxAcc, used))
                                  build sigAcc ctxAcc used (n:ns) = do
                                    let sig' = CCG.sig n ++ sigAcc
                                        tcQuery = UDTT.Judgment sig' ctxAcc (CCG.sem n) DTT.Type
                                        lts = TY.typeCheck prover verbose tcQuery
                                    mu <- LT.uncons lts
                                    case mu of
                                      Nothing -> return Nothing
                                      Just (d, _) -> do
                                        let t = DTT.trm (Tree.node d)
                                            ctx' = t : ctxAcc
                                        build sig' ctx' (t:used) ns
                              mres <- build baseSig baseCtx [] nodes
                              case mres of
                                Nothing -> return ()
                                Just (sigAccum, ctxAccum, usedTerms) -> do
                                  let typ  = case ctxAccum of { (t:_) -> t; [] -> DTT.Bot }
                                      rest = case ctxAccum of { (_:xs) -> xs; [] -> [] }
                                      sigNorm = normalizeSignature sigAccum
                                      psqPos = DTT.ProofSearchQuery sigNorm rest typ
                                      psqNeg = DTT.ProofSearchQuery sigNorm rest (DTT.Pi typ DTT.Bot)
                                      keyPos = Store.encode psqPos
                                      keyNeg = Store.encode psqNeg
                                  -- 既存キャッシュがなければ流し込む
                                  mp <- readIORef proofCacheRef
                                  when (M.notMember keyPos mp) $ do
                                    _ <- scheduleLimited $ do
                                      -- extra に使用済み図の項を渡す（照合用）
                                      atomicModifyIORef' proofCacheRef (\m0 ->
                                        let ce0 = M.findWithDefault emptyCacheEntry keyPos m0
                                            ce1 = ce0 { cacheExtraHashes = cacheExtraHashes ce0 ++ map Store.encode usedTerms }
                                        in (M.insert keyPos ce1 m0, ()))
                                      let loop l = do
                                            m <- LT.uncons l
                                            case m of
                                              Nothing -> atomicModifyIORef' proofCacheRef (\m0 ->
                                                            let ce = M.findWithDefault emptyCacheEntry keyPos m0
                                                                ce' = ce { cachePosDone = True }
                                                            in (M.insert keyPos ce' m0, ()))
                                              Just (d, restL) -> do
                                                atomicModifyIORef' proofCacheRef (\m0 ->
                                                  let ce = M.findWithDefault emptyCacheEntry keyPos m0
                                                      ce' = ce { cachePos = cachePos ce ++ [d] }
                                                  in (M.insert keyPos ce' m0, ()))
                                                loop restL
                                      loop (LT.take nProof (prover psqPos))
                                    return ()
                                  mn <- readIORef proofCacheRef
                                  when (M.notMember keyNeg mn) $ do
                                    _ <- scheduleLimited $ do
                                      atomicModifyIORef' proofCacheRef (\m0 ->
                                        let ce0 = M.findWithDefault emptyCacheEntry keyNeg m0
                                            ce1 = ce0 { cacheExtraHashes = cacheExtraHashes ce0 ++ map Store.encode usedTerms }
                                        in (M.insert keyNeg ce1 m0, ()))
                                      let loop l = do
                                            m <- LT.uncons l
                                            case m of
                                              Nothing -> atomicModifyIORef' proofCacheRef (\m0 ->
                                                            let ce = M.findWithDefault emptyCacheEntry keyNeg m0
                                                                ce' = ce { cacheNegDone = True }
                                                            in (M.insert keyNeg ce' m0, ()))
                                              Just (d, restL) -> do
                                                atomicModifyIORef' proofCacheRef (\m0 ->
                                                  let ce = M.findWithDefault emptyCacheEntry keyNeg m0
                                                      ce' = ce { cacheNeg = cacheNeg ce ++ [d] }
                                                  in (M.insert keyNeg ce' m0, ()))
                                                loop restL
                                      loop (LT.take nProof (prover psqNeg))
                                    return ()
                    else threadDelay 300000 >> waitLoop
        waitLoop
  _ <- forkIO prewarmBest
  return ()

  -- 複数経路探索
  _ <- forkIO $ do
    let waitAllParsed = do
          md <- readIORef currentDiscourseNodesRef
          case md of
            Nothing -> threadDelay 300000 >> waitAllParsed
            Just disc -> if all snDone disc then return () else threadDelay 300000 >> waitAllParsed
    waitAllParsed
    mProver <- readIORef currentProverRef
    baseSig <- readIORef currentBaseSignatureRef
    baseCtx <- readIORef currentBaseContextRef
    nProof  <- readIORef currentNProofRef
    nType   <- readIORef currentNTypeCheckRef
    verbose <- readIORef currentVerboseRef
    kTop <- readIORef currentPrewarmTopKRef
    case mProver of
      Nothing -> return ()
      Just prover -> do
        Just disc <- readIORef currentDiscourseNodesRef
        -- 各文で score 降順に top-k を選択
        let topNodesPerSentence :: [[CCG.Node]]
            topNodesPerSentence = map (take kTop . reverse . sortOn CCG.score . snNodes) disc
        let total = length topNodesPerSentence
        let build sigAcc ctxAcc idx = 
              if idx >= total
                then do
                  let typ  = case ctxAcc of { (t:_) -> t; [] -> DTT.Bot }
                      rest = case ctxAcc of { (_:xs) -> xs; [] -> [] }
                      psqPos = DTT.ProofSearchQuery sigAcc rest typ
                      psqNeg = DTT.ProofSearchQuery sigAcc rest (DTT.Pi typ DTT.Bot)
                  ensureCached psqPos nProof prover
                  ensureCached psqNeg nProof prover
                else do
                  let nodesHere = topNodesPerSentence !! idx
                  mapM_ (\n -> do
                           let sig' = CCG.sig n ++ sigAcc
                               tcQuery = UDTT.Judgment sig' ctxAcc (CCG.sem n) DTT.Type
                               lts = TY.typeCheck prover verbose tcQuery
                           diags <- LT.toList (LT.take nType lts)
                           mapM_ (\d -> do
                                   let ctx' = (DTT.trm (Tree.node d)) : ctxAcc
                                   build sig' ctx' (idx+1)
                                 ) diags
                        ) nodesHere
        build baseSig baseCtx 0
  return ()

  let port = 3000
  mStart <- lookupEnv "LB_EXPRESS_START"
  let startPath = case mStart of
                    Just s | map toLower s == "inference" -> "/inference"
                    _ -> "/error"
  let url = "http://localhost:" ++ show port ++ startPath

  -- ブラウザ選択
  mBrowser <- lookupEnv "LB_EXPRESS_BROWSER"
  let browserSel = fmap (map toLower) mBrowser
      openBrowserCommand =
        case os of
          "darwin" ->
            case browserSel of
              Just "chrome"  -> "open -a \"Google Chrome\" " ++ url
              Just "firefox" -> "open -a \"Firefox\" " ++ url
              _              -> "open " ++ url
          "linux"  ->
            case browserSel of
              Just "chrome"  -> "google-chrome " ++ url ++ " || google-chrome-stable " ++ url ++ " || chromium " ++ url ++ " || chromium-browser " ++ url ++ " || xdg-open " ++ url
              Just "firefox" -> "firefox " ++ url ++ " || xdg-open " ++ url
              _              -> "xdg-open " ++ url
          "mingw32" ->
            case browserSel of
              Just "chrome"  -> "start chrome " ++ url
              Just "firefox" -> "start firefox " ++ url
              _              -> "start " ++ url
          _        -> "echo 'Unsupported OS for auto-opening browser.'"

  putStrLn $ "Starting Yesod server on " ++ url

  _ <- forkIO $ do
    callCommand openBrowserCommand `catch` \e -> do
      hPutStrLn stderr $ "Failed to open browser: " ++ show (e :: IOException)

  warp port App

getInferenceR :: Handler Html
getInferenceR = do
  -- 逐次パースを取得
  mDisc <- liftIO $ readIORef currentDiscourseNodesRef
  case mDisc of
    Nothing -> defaultLayout $ do
      [whamlet|
        <div class="error-message">
          <p>No discourse parsed yet. Please (re)start with JSeM express.
      |]
      myDesign
      myFunction
    Just discourse -> do
      dsp <- liftIO $ readIORef currentDisplaySettingRef
      sel <- liftIO $ readIORef currentTCSelectionRef
      let isAllowed s = all (\i -> i <= 0 || M.member i sel) [s-1]
          selNodeIdxFor :: Int -> Int
          selNodeIdxFor s = case M.lookup s sel of
                              Just s' | selNodeIdx s' > 0 -> selNodeIdx s'
                              _ -> 0
      let enumerated = zip ([1..] :: [Int]) discourse
          enumeratedWithNodes :: [(Int, SentenceProgress, [(Int, CCG.Node)])]
          enumeratedWithNodes =
            [ (sidx, sp, Prelude.zip [1..] (snNodes sp))
            | (sidx, sp) <- enumerated
            ]
          nTotal :: Int
          nTotal = length discourse
          prefixFor :: Int -> TS.Text
          prefixFor i = if i == nTotal then "Hypothesis: " else TS.pack ("Premise" ++ show i ++ ": ")
      -- カラム表示: 各入力文を1カラムとして、その下に nparse 個の Node を表示
      defaultLayout $ do
        [whamlet|
          <div class="inference-container">
            <div #inference-grid>
              $forall (sidx, sp, nodesZ) <- enumeratedWithNodes
                <div .inf-col data-sidx=#{sidx}>
                  <div .inf-col-head>
                    <span .inf-head-title>#{prefixFor sidx}#{T.toStrict (snText sp)}
                    $if sidx == nTotal
                      $with hasSel <- M.member sidx sel
                        <span .inf-head-actions>
                          $if hasSel
                            <button .btn .btn-select onclick="goProofsearch()">proofsearch
                          $else
                            <button .btn .btn-select .is-disabled aria-disabled=true data-disabled=1 title="入力文のTypeCheckDiagramを選択してください。">proofsearch
                  <div .inf-col-body>
                    $if null (snNodes sp)
                      <div .span-preview-loading>loading...
                    $else
                      $forall (nidx, node) <- nodesZ
                        <div .inf-node-item :nidx == selNodeIdxFor sidx:.selected>
                          <div .inf-node-score>score: #{T.toStrict $ CCG.showScore node}
                          <div .inf-node-actions>
                            $if isAllowed sidx
                              <button .btn .btn-run data-sidx=#{sidx} onclick="startTypecheck(this)">typecheck
                            $else
                              <button .btn .btn-run .is-disabled aria-disabled=true data-disabled=1 data-sidx=#{sidx} title="前文のTypeCheckDiagramを選択してください。">typecheck
                          ^{WE.widgetizeWith dsp node}
                          <div .inf-node-tc .tc-holder>
                    $if not (snDone sp)
                      <div .span-preview-loading>loading...
        |]
        myDesign
        myFunction

-- 進捗の簡易JSON（全カラムが完了したか）
getInfProgressR :: Handler Value
getInfProgressR = do
  mDisc <- liftIO $ readIORef currentDiscourseNodesRef
  case mDisc of
    Nothing -> return $ object ["allDone" .= True]
    Just disc -> return $ object ["allDone" .= all snDone disc]

-- 各カラム（文インデックスごと）の現在ノードをHTMLスニペットで返す
getInfColR :: Handler Html
getInfColR = do
  mDisc <- liftIO $ readIORef currentDiscourseNodesRef
  mSent <- lookupGetParam "sent"
  let parseInt :: TS.Text -> Maybe Int
      parseInt = readMaybe . TS.unpack
      sIdx = maybe 1 id (mSent >>= parseInt)
      s0 = sIdx - 1
      (!!?) :: [a] -> Int -> Maybe a
      (!!?) xs n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)
  case mDisc >>= (!!? s0) of
    Nothing -> return [shamlet|<div .span-preview-loading>loading...|]
    Just sp -> do
      dsp <- liftIO $ readIORef currentDisplaySettingRef
      sel <- liftIO $ readIORef currentTCSelectionRef
      let isAllowed s = all (\i -> i <= 0 || M.member i sel) [s-1]
          selNodeIdxForThis :: Int
          selNodeIdxForThis = case M.lookup sIdx sel of
                                Just s | selNodeIdx s > 0 -> selNodeIdx s
                                _ -> 0
          enumeratedNodes :: [(Int, CCG.Node)]
          enumeratedNodes = Prelude.zip ([1..] :: [Int]) (snNodes sp)
          totalSentences :: Int
          totalSentences = maybe 0 length mDisc
          hasSelFinal :: Bool
          hasSelFinal = M.member sIdx sel
      defaultLayout $ do
        if null (snNodes sp)
          then [whamlet|<div .span-preview-loading>loading...|]
          else do
            [whamlet|
              $forall (nidx, node) <- enumeratedNodes
                <div .inf-node-item :nidx == selNodeIdxForThis:.selected>
                  <div .inf-node-score>score: #{T.toStrict $ CCG.showScore node}
                  <div .inf-node-actions>
                    $if isAllowed sIdx
                      <button .btn .btn-run data-sidx=#{sIdx} onclick="startTypecheck(this)">typecheck
                    $else
                      <button .btn .btn-run .is-disabled aria-disabled=true data-disabled=1 data-sidx=#{sIdx} title="前文のTypeCheckDiagramを選択してください。">typecheck
                  ^{WE.widgetizeWith dsp node}
                  <div .inf-node-tc .tc-holder>
            |]
        when (not $ snDone sp) $
          [whamlet|<div .span-preview-loading>loading...|]


-- printExpressInterface を showExpress に変更し、ParseResult を引数に取る
showExpress :: NLI.ParseResult -> IO ()
showExpress initialParseResult = do
  -- 初期ParseResultをIORefに保存
  atomicWriteIORef currentParseResultRef (Just initialParseResult)

  -- 環境変数から表示設定を読み込み、反映
  applyEnvDisplayOptions

  let port = 3000
  mStart <- lookupEnv "LB_EXPRESS_START"
  let startPath = case mStart of
                    Just s | map toLower s == "parsing" -> "/parsing"
                    _ -> "/error"
  let url = "http://localhost:" ++ show port ++ startPath

  -- ブラウザ選択（環境変数 LB_EXPRESS_BROWSER: chrome|firefox|default）
  mBrowser <- lookupEnv "LB_EXPRESS_BROWSER"
  let browserSel = fmap (map toLower) mBrowser
      openBrowserCommand =
        case os of
          "darwin" ->
            case browserSel of
              Just "chrome"  -> "open -a \"Google Chrome\" " ++ url
              Just "firefox" -> "open -a \"Firefox\" " ++ url
              _              -> "open " ++ url
          "linux"  ->
            case browserSel of
              Just "chrome"  -> "google-chrome " ++ url ++ " || google-chrome-stable " ++ url ++ " || chromium " ++ url ++ " || chromium-browser " ++ url ++ " || xdg-open " ++ url
              Just "firefox" -> "firefox " ++ url ++ " || xdg-open " ++ url
              _              -> "xdg-open " ++ url
          "mingw32" ->
            case browserSel of
              Just "chrome"  -> "start chrome " ++ url
              Just "firefox" -> "start firefox " ++ url
              _              -> "start " ++ url
          _        -> "echo 'Unsupported OS for auto-opening browser.'"

  putStrLn $ "Starting Yesod server on " ++ url

  _ <- forkIO $ do
    callCommand openBrowserCommand `catch` \e -> do
      hPutStrLn stderr $ "Failed to open browser: " ++ show (e :: IOException)

  warp port App

-- 環境変数 LB_EXPRESS_DEPTH / LB_EXPRESS_NOSHOWCAT / LB_EXPRESS_NOSHOWSEM から設定を反映
applyEnvDisplayOptions :: IO ()
applyEnvDisplayOptions = do
  mDepthStr <- lookupEnv "LB_EXPRESS_DEPTH"
  let mDepth = mDepthStr >>= readMaybe
  mNoShowCat <- lookupEnv "LB_EXPRESS_NOSHOWCAT"
  mNoShowSem <- lookupEnv "LB_EXPRESS_NOSHOWSEM"
  mLeafVertical <- lookupEnv "LB_EXPRESS_LEAFVERTICAL"
  let toBool v = case v of { Just "1" -> True; Just "true" -> True; Just "True" -> True; _ -> False }
      noShowCat = toBool mNoShowCat
      noShowSem = toBool mNoShowSem
      leafVert = toBool mLeafVertical
      base = WE.defaultDisplaySetting
      base' = maybe base (\d -> base { WE.defaultExpandDepth = d }) mDepth
      base'' = if noShowCat then base' { WE.showCat = False } else base'
      base''' = if noShowSem then base'' { WE.showSem = False } else base''
      dsp = base''' { WE.leafVertical = leafVert }
  atomicWriteIORef currentDisplaySettingRef dsp

getErrorR :: Handler Html
getErrorR = do
  defaultLayout $ do
    [whamlet|
      <div style="padding:16px">
        <h1>Error
        <p>Invalid request
    |]

getParsingR :: Handler Html
getParsingR = do
     -- IORef から ParseResult を読み込む
     mpr <- liftIO $ readIORef currentParseResultRef
     case mpr of
        Nothing -> do
          -- 初期値が設定されていない場合はエラーメッセージを表示
          defaultLayout $ do
            [whamlet|
              <div class="error-message">
                <p>ParseResult is not set...
            |]
        Just pr -> do
          let text_sen = case pr of
                           NLI.SentenceAndParseTrees sentence _ -> sentence
                           _ -> T.empty
          -- tabs個のnode
          -- parseSentence' :: NLI.ParseResult -> IO ([CCG.Node])
          nodes <- liftIO $ L.parseSentence' pr

          -- 各 node ごとの葉ノードを取得
          let leafNodesList = map CCG.getLeafNodesFromNode nodes

          let scores = Prelude.map CCG.showScore nodes

          -- タブ数はノード数に合わせる
          let numTabs = Prelude.length nodes
          let tabs = [1..numTabs]

          -- tabs個のType Check Query
          -- parseSentenceForQuery :: NLI.ParseResult -> IO ([UDTT.TypeCheckQuery])
          tcqs <- liftIO $ L.parseSentenceForQuery pr
     
          -- tcds 
          -- parseSentenceForDiagram :: NLI.ParseResult -> IO ([[QT.DTTProofDiagram]])
          tcds <- liftIO $ L.parseSentenceForDiagram pr

          let tcdLengths = Prelude.map Prelude.length tcds
          liftIO $ putStrLn $ "length: " ++ show tcdLengths

          let tabClasses = Prelude.map (\tcdList -> if Data.List.null tcdList then "tab-label error" :: T.Text else "tab-label" :: T.Text) tcds
          
          -- 表示設定（CLI等から設定された値を参照）
          dsp <- liftIO $ readIORef currentDisplaySettingRef
          let catChecked = not (WE.showCat dsp)
          let semChecked = not (WE.showSem dsp)

          defaultLayout $ do
            [whamlet|
              <div id="parsing-view" style="display:block;">
                <div class="header">
                  <div class="parsing-content">
                    <div class="sentence-line">sentence: <span id="sentence-text">#{text_sen}</span>
                    <div class="span-controls">
                      <input id="span-surface" type="text" placeholder="部分文字列をドラッグ選択">
                      <input id="span-beam" type="number" value="32" min="1" max="256">
                      <select id="span-results"></select>
                      <span id="span-status"></span>
                    <div class="toggle-group">
                      <label for="cat-toggle" id="catbtn" class="toggle">cat
                      <label for="sem-toggle" id="sembtn" class="toggle">sem

                <input type="checkbox" id="cat-toggle" :catChecked:checked/>
                <input type="checkbox" id="sem-toggle" :semChecked:checked/>

                <div id="span-preview-title" class="span-preview-title"></div>
                <div id="span-preview-list" class="span-preview-list">

                <div class="container-tab">
                  <div .tab-wrap>
                    $forall (tabNum, node, tcq, tcdList, tabClass, score, leafNodes) <- Data.List.zip7 tabs nodes tcqs tcds tabClasses scores leafNodesList
                      <input id="TAB-#{tabNum}" type="radio" name="TAB" class="tab-switch" :tabNum == 1:checked>
                      <label for="TAB-#{tabNum}" class=#{tabClass}>#{tabNum} (score: #{score})
                      <div class="tab-content">
                        <div class="tab-leaves">
                          <h2>Lexical Items
                          <div .leaf-node-list :WE.leafVertical dsp:.vertical>
                            $forall leaf <- leafNodes
                              <div .leaf-node-item>^{WE.widgetizeWith dsp leaf}
                        <div class="tab-node">
                          <h1>Syntactic Structures
                        <div class="tab-node-content">
                          <div .tab-node-inner>^{WE.widgetizeWith dsp node}
                        <div class="tab-tcq">
                          <h1>Type Check Query
                          <div class="tab-tcq-content">^{WE.widgetizeWith dsp tcq}
                        <div class="tab-tcd">
                          <h1>Type Check Diagram
                          $if Data.List.null tcdList
                            <p .error-message>⚠️ Type Check Failed... ⚠️
                          $else
                            <div class="tab-tcds-content">
                              <div .tab-tcds-inner>^{Prelude.mapM_ (WE.widgetizeWith dsp) $ tcdList}
            |]
            myDesign
            myFunction
 
-- JSON: span parse results
getSpanR :: Handler Value
getSpanR = do
  mpr <- liftIO $ readIORef currentParseResultRef
  case mpr of
    Nothing -> return $ object ["error" .= ("ParseResult is not set" :: TS.Text)]
    Just pr -> do
      let text_sen = case pr of
                        NLI.SentenceAndParseTrees sentence _ -> sentence
                        _ -> T.empty
      mSurface <- lookupGetParam "surface"
      mStart <- lookupGetParam "start"
      mEnd <- lookupGetParam "end"
      mBeam <- lookupGetParam "beam"
      let beam :: Int
          beam = case mBeam >>= (readMaybe . TS.unpack) of
                   Just b -> b
                   Nothing -> 32
      chart <- liftIO $ L.parse beam text_sen
      let entries = M.toList chart
          normalize :: TS.Text -> TS.Text
          normalize = TS.filter (\c -> not (c `elem` delims))
          delims :: [Char]
          delims = [' ', '　', '。', '、', '，', '．', ',', '.', '!', '！', '?', '？']
      -- Token-based span finder: substring -> (i,j)
      jpOptions <- liftIO PL.defaultJpOptions
      (tokens, _) <- liftIO $ JP.setupLexicon jpOptions text_sen
      let tokensN = map (normalize . T.toStrict) tokens
          nTok = length tokensN
          matchSpan :: TS.Text -> Maybe (Int,Int)
          matchSpan target
            | TS.null target = Nothing
            | otherwise =
                let go s e = if s >= nTok then Nothing
                              else if e > nTok then go (s+1) (s+2)
                              else let catse = TS.concat (take (e-s) (drop s tokensN)) in
                                   if catse == target then Just (s,e) else go s (e+1)
                in go 0 1
          pickBySurface :: TS.Text -> Maybe ((Int,Int), [CP.Node])
          pickBySurface surf =
            let target = normalize surf in
            let matches = [ (ij, ns)
                          | (ij, ns) <- entries
                          , not (null ns)
                          , let pfs = map (normalize . T.toStrict . CCG.pf) ns
                          , any (== target) pfs
                          ] in
            case matches of
              (x:_) -> Just x
              _ -> Nothing
          pickByIndex :: Int -> Int -> [((Int,Int), [a])] -> Maybe ((Int,Int), [a])
          pickByIndex i j = find (\((i',j'),_) -> i'==i && j'==j)
      chosen <- case (mStart >>= readMaybe . TS.unpack, mEnd >>= readMaybe . TS.unpack) of
                  (Just i, Just j) -> return $ pickByIndex i j entries
                  _ -> case mSurface of
                         Just surf ->
                           case matchSpan (normalize surf) of
                             Just (i,j) -> return $ pickByIndex i j entries
                             Nothing    -> return $ pickBySurface surf
                         Nothing -> return Nothing
      case chosen of
        Nothing -> do
          -- Fallback: parse the substring independently
          case mSurface of
            Nothing -> return $ object ["nodes" .= ([] :: [TS.Text]), "message" .= ("No match" :: TS.Text)]
            Just surf -> do
              subChart <- liftIO $ L.parse beam (T.fromStrict surf)
              let prx = Partial.extractParseResult beam subChart
              case prx of
                Partial.Failed -> return $ object ["nodes" .= ([] :: [TS.Text]), "message" .= ("No match" :: TS.Text)]
                Partial.Full ns -> do
                  let toNodeObj n = object
                        [ "pf" .= (T.toStrict $ CCG.pf n)
                        , "cat" .= (TS.pack $ show $ CCG.cat n)
                        , "score" .= (T.toStrict $ CCG.showScore n)
                        ]
                  return $ object
                    [ "pf" .= surf
                    , "nodes" .= map toNodeObj ns
                    , "subparse" .= True
                    ]
                Partial.Partial ns -> do
                  let toNodeObj n = object
                        [ "pf" .= (T.toStrict $ CCG.pf n)
                        , "cat" .= (TS.pack $ show $ CCG.cat n)
                        , "score" .= (T.toStrict $ CCG.showScore n)
                        ]
                  return $ object
                    [ "pf" .= surf
                    , "nodes" .= map toNodeObj ns
                    , "subparse" .= True
                    ]
        Just ((i,j), ns) -> do
          let toNodeObj n = object
                [ "pf" .= (T.toStrict $ CCG.pf n)
                , "cat" .= (TS.pack $ show $ CCG.cat n)
                , "score" .= (T.toStrict $ CCG.showScore n)
                ]
          return $ object
            [ "span" .= object ["start" .= i, "end" .= j]
            , "pf" .= (T.toStrict $ CCG.pf (head ns))
            , "nodes" .= map toNodeObj ns
            , "subparse" .= False
            ]


--CSS（cassius）
myDesign :: Widget
myDesign = do
    toWidget $(cassiusFile "src/Interface/Express/templates/common.cassius")
    toWidget $(cassiusFile "src/Interface/Express/templates/parsing.cassius")
    toWidget $(cassiusFile "src/Interface/Express/templates/inference.cassius")
    toWidget $(cassiusFile "src/Interface/Express/templates/proofsearch.cassius")
    toWidget $(cassiusFile "src/Interface/Express/templates/toast.cassius")

-- julius file for javascript
myFunction :: Widget
myFunction = do
    toWidget $(juliusFile "src/Interface/Express/templates/express.julius")

-- Build proof search queries from current selections and show progressive page
getProofSearchR :: Handler Html
getProofSearchR = do
  mDisc <- liftIO $ readIORef currentDiscourseNodesRef
  selMap <- liftIO $ readIORef currentTCSelectionRef
  baseSig <- liftIO $ readIORef currentBaseSignatureRef
  baseCtx <- liftIO $ readIORef currentBaseContextRef
  mProver <- liftIO $ readIORef currentProverRef
  nProof <- liftIO $ readIORef currentNProofRef
  -- reset state
  liftIO $ atomicWriteIORef currentPSQPosRef Nothing
  liftIO $ atomicWriteIORef currentPSQNegRef Nothing
  liftIO $ atomicWriteIORef currentPSPosRef []
  liftIO $ atomicWriteIORef currentPSNegRef []
  liftIO $ atomicWriteIORef currentPSDonePosRef False
  liftIO $ atomicWriteIORef currentPSDoneNegRef False
  case (mDisc, mProver) of
    (Just discourse, Just prover) -> do
      -- Build signature and context from selections
      let total = length discourse
          -- aggregate selected nodes for signature
          getNodeAt sidx = do
            let s0 = sidx - 1
            case discourse !!? s0 of
              Nothing -> Nothing
              Just sp ->
                case M.lookup sidx selMap of
                  Nothing -> Nothing
                  Just sel ->
                    let n0 = selNodeIdx sel - 1
                    in (snNodes sp) !!? n0
          sigAccum = baseSig ++ concat [ maybe [] CCG.sig (getNodeAt i) | i <- [1..total] ]
          -- build context with last chosen term at head
          ctxAccum = foldl (\acc i ->
                               case M.lookup i selMap of
                                 Just s -> (DTT.trm (Tree.node (selDiagram s))):acc
                                 Nothing -> acc) baseCtx [1..total]
      -- Only if last sentence selected
      if M.member total selMap
        then do
          let typ = case ctxAccum of
                      (t:_) -> t
                      []    -> DTT.Bot
              rest = case ctxAccum of
                       (_:xs) -> xs
                       []     -> []
              sigNorm = normalizeSignature sigAccum
              psqPos = DTT.ProofSearchQuery sigNorm rest typ
              psqNeg = DTT.ProofSearchQuery sigNorm rest (DTT.Pi typ DTT.Bot)
              enumerated :: [(Int, SentenceProgress)]
              enumerated = zip ([1..] :: [Int]) discourse
          liftIO $ atomicWriteIORef currentPSQPosRef (Just psqPos)
          liftIO $ atomicWriteIORef currentPSQNegRef (Just psqNeg)
          -- augment cache entries with full context terms so diagrams can be matched
          liftIO $ addTermsToCache psqPos ctxAccum
          liftIO $ addTermsToCache psqNeg ctxAccum
          -- Check cache and seed current results if present
          let keyPos = Store.encode psqPos
              keyNeg = Store.encode psqNeg
          mCachedPos <- liftIO $ readIORef proofCacheRef >>= \m -> return (M.lookup keyPos m)
          mCachedNeg <- liftIO $ readIORef proofCacheRef >>= \m -> return (M.lookup keyNeg m)
          case mCachedPos of
            Just ce -> do
              liftIO $ atomicWriteIORef currentPSPosRef (cachePos ce)
              liftIO $ atomicWriteIORef currentPSDonePosRef (cachePosDone ce)
            Nothing -> return ()
          case mCachedNeg of
            Just ce -> do
              liftIO $ atomicWriteIORef currentPSNegRef (cacheNeg ce)
              liftIO $ atomicWriteIORef currentPSDoneNegRef (cacheNegDone ce)
            Nothing -> return ()
          -- NOTE: 実探索の起動はページ描画後に /proofsearch/start から行う（初期レスポンスを軽くする）
          defaultLayout $ do
            [whamlet|
              <div .ps-container>
                <div .ps-header>
                  <div .ps-header-title>Proof Search
                  <div .ps-header-ctl>
                    <span #ps-outcome .ps-outcome>Searching...
                  <div .ps-sentences>
                    $forall (i, sp) <- enumerated
                      <div .ps-row>
                        <div .ps-role>
                          $if i == total
                            Hypothesis
                          $else
                            Premise #{i}
                        <div .ps-text>#{T.toStrict (snText sp)}
                <div .ps-grid>
                  <div .ps-section>
                    <div .ps-title>
                      <span .ps-badge .ps-badge-pos>Query (pos)
                      <a .btn .btn-save href=@{ProofQueryBinR}?kind=pos download>保存
                    <div .ps-body>
                      <pre id="psq-pos">loading...
                  <div .ps-section>
                    <div .ps-title>
                      <span .ps-badge .ps-badge-neg>Query (neg)
                      <a .btn .btn-save href=@{ProofQueryBinR}?kind=neg download>保存
                    <div .ps-body>
                      <pre id="psq-neg">loading...
                  <div .ps-section>
                    <div .ps-title>
                      <span .ps-badge .ps-badge-pos>Results (pos)
                      <span #ps-pos-status .ps-status>Searching...
                    <div .ps-body>
                      <div class="tab-tcds-content">
                        <div id="ps-pos-list" class="tc-holder">
                          <div class="span-preview-loading">loading...</div>
                  <div .ps-section>
                    <div .ps-title>
                      <span .ps-badge .ps-badge-neg>Results (neg)
                      <span #ps-neg-status .ps-status>Searching...
                    <div .ps-body>
                      <div class="tab-tcds-content">
                        <div id="ps-neg-list" class="tc-holder">
                          <div class="span-preview-loading">loading...</div>
            |]
            myDesign
            myFunction
        else defaultLayout [whamlet|<div class="error-message">Select final diagram first|]
    _ -> defaultLayout [whamlet|<div class="error-message">Not ready|]
  where
    (!!?) :: [a] -> Int -> Maybe a
    (!!?) xs n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)

getProofProgressR :: Handler Value
getProofProgressR = do
  qpos <- liftIO $ readIORef currentPSQPosRef
  qneg <- liftIO $ readIORef currentPSQNegRef
  pos <- liftIO $ readIORef currentPSPosRef
  neg <- liftIO $ readIORef currentPSNegRef
  dpos <- liftIO $ readIORef currentPSDonePosRef
  dneg <- liftIO $ readIORef currentPSDoneNegRef
  return $ object
    [ "hasPos" .= (maybe False (const True) qpos)
    , "hasNeg" .= (maybe False (const True) qneg)
    , "posCount" .= length pos
    , "negCount" .= length neg
    , "posDone" .= dpos
    , "negDone" .= dneg
    ]

-- Proof precompute cache progress (for /inference mini indicator)
getProofCacheProgressR :: Handler Value
getProofCacheProgressR = do
  m <- liftIO $ readIORef proofCacheRef
  let entries = M.elems m
      posFound = sum (map (length . cachePos) entries)
      negFound = sum (map (length . cacheNeg) entries)
      posDoneN = length (filter cachePosDone entries)
      negDoneN = length (filter cacheNegDone entries)
      totalKeys = M.size m
  return $ object
    [ "keys" .= totalKeys
    , "pos" .= posFound
    , "neg" .= negFound
    , "posDone" .= posDoneN
    , "negDone" .= negDoneN
    ]

-- For a sentence: return per node/diag the POS/NEG/running flags if any cached PSQ includes that diagram's term
getProofCacheMarksR :: Handler Value
getProofCacheMarksR = do
  mDisc <- liftIO $ readIORef currentDiscourseNodesRef
  mSent <- lookupGetParam "sent"
  let parseInt :: TS.Text -> Maybe Int
      parseInt = readMaybe . TS.unpack
      sIdx = maybe 1 id (mSent >>= parseInt)
      s0 = sIdx - 1
      (!!?) :: [a] -> Int -> Maybe a
      (!!?) xs n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)
  case mDisc >>= (!!? s0) of
    Nothing -> return $ object ["items" .= ([] :: [Value])]
    Just sp -> do
      st <- liftIO $ readIORef currentTCStateRef
      let nodeCount = length (snNodes sp)
          nodeIdxs = [1..nodeCount] :: [Int]
          pick k = M.lookup (sIdx, k) st
      let mkItem nidx didx hasP hasN runP runN =
            object [ "sidx" .= sIdx
                   , "nidx" .= nidx
                   , "didx" .= didx
                   , "pos"  .= hasP
                   , "neg"  .= hasN
                   , "runPos" .= runP
                   , "runNeg" .= runN
                   ]
      cache <- liftIO $ readIORef proofCacheRef
      let entries = M.elems cache
          -- helper: check diagram term hash against cache entry contexts (including extra/head terms)
          check termHash ce =
            let hit = any (== termHash) (cacheCtxHashes ce) || any (== termHash) (cacheExtraHashes ce)
            in if not hit then (False, False, False, False)
               else
                 let hasP = not (null (cachePos ce))
                     hasN = not (null (cacheNeg ce))
                     runP = not (cachePosDone ce) && null (cachePos ce)
                     runN = not (cacheNegDone ce) && null (cacheNeg ce)
                 in (hasP, hasN, runP, runN)
      let items =
            concatMap (\nidx ->
              case pick nidx of
                Just (TCDone diags) ->
                  let indexed = zip ([1..] :: [Int]) diags
                  in map (\(didx, d) ->
                           let term = DTT.trm (Tree.node d)
                               thash = Store.encode term
                               results = map (check thash) entries
                               anyP = or [p | (p,_,_,_) <- results]
                               anyN = or [n | (_,n,_,_) <- results]
                               anyRunP = or [rp | (_,_,rp,_) <- results]
                               anyRunN = or [rn | (_,_,_,rn) <- results]
                           in mkItem nidx didx anyP anyN anyRunP anyRunN
                         ) indexed
                _ -> []
            ) nodeIdxs
      return $ object ["items" .= items]

getProofListR :: Handler Html
getProofListR = do
  mk <- lookupGetParam "kind"
  dsp <- liftIO $ readIORef currentDisplaySettingRef
  case mk of
    Just "pos" -> do
      pos <- liftIO $ readIORef currentPSPosRef
      let indexed = zip ([1..] :: [Int]) pos
      defaultLayout $ do
        [whamlet|
          <div class="tab-tcds-content">
            $forall (idx, d) <- indexed
              <div .tab-tcds-inner data-diag-idx=#{idx}>^{WE.widgetizeWith dsp d}
        |]
    Just "neg" -> do
      neg <- liftIO $ readIORef currentPSNegRef
      let indexed = zip ([1..] :: [Int]) neg
      defaultLayout $ do
        [whamlet|
          <div class="tab-tcds-content">
            $forall (idx, d) <- indexed
              <div .tab-tcds-inner data-diag-idx=#{idx}>^{WE.widgetizeWith dsp d}
        |]
    _ -> defaultLayout [whamlet|<div class="error-message">invalid kind|]

getProofQueryR :: Handler Html
getProofQueryR = do
  mk <- lookupGetParam "kind"
  q <- case mk of
         Just "pos" -> liftIO $ readIORef currentPSQPosRef
         Just "neg" -> liftIO $ readIORef currentPSQNegRef
         _          -> return Nothing
  dsp <- liftIO $ readIORef currentDisplaySettingRef
  case q of
    Nothing -> defaultLayout [whamlet|<pre>loading...</pre>|]
    Just psq -> do
      -- Convert deBruijn to with-name for display
      let psqWN = DWN.fromDeBruijnProofSearchQuery psq
      defaultLayout $ do
        [whamlet|
          <div class="tab-tcq-content-inference">^{WE.widgetizeWith dsp psqWN}
        |]

-- Start proving (pos/neg) lazily after /proofsearch renders
getProofStartR :: Handler Value
getProofStartR = do
  mk <- lookupGetParam "kind"
  nProof <- liftIO $ readIORef currentNProofRef
  mProver <- liftIO $ readIORef currentProverRef
  case (mk, mProver) of
    (Just "pos", Just prover) -> do
      mpsq <- liftIO $ readIORef currentPSQPosRef
      case mpsq of
        Nothing -> return $ object ["status" .= ("no_psq" :: TS.Text)]
        Just psq -> do
          let key = Store.encode psq
          cache <- liftIO $ readIORef proofCacheRef
          let already = maybe False cachePosDone (M.lookup key cache) || maybe False (not . null . cachePos) (M.lookup key cache)
          if already
            then return $ object ["status" .= ("cached" :: TS.Text)]
            else do
              _ <- liftIO $ scheduleLimited $ do
                let l = LT.take nProof (prover psq)
                m <- LT.uncons l
                case m of
                  Nothing -> atomicModifyIORef' proofCacheRef (\m0 ->
                              let ce = M.findWithDefault emptyCacheEntry key m0
                                  ce' = ce { cachePosDone = True }
                              in (M.insert key ce' m0, ()))
                  Just (d, _) -> do
                    atomicModifyIORef' proofCacheRef (\m0 ->
                      let ce = M.findWithDefault emptyCacheEntry key m0
                          ce' = ce { cachePos = cachePos ce ++ [d], cachePosDone = True }
                      in (M.insert key ce' m0, ()))
                    atomicModifyIORef' currentPSPosRef (\xs -> (xs ++ [d], ()))
                    atomicWriteIORef currentPSDonePosRef True
              return $ object ["status" .= ("started" :: TS.Text)]
    (Just "neg", Just prover) -> do
      mpsq <- liftIO $ readIORef currentPSQNegRef
      case mpsq of
        Nothing -> return $ object ["status" .= ("no_psq" :: TS.Text)]
        Just psq -> do
          let key = Store.encode psq
          cache <- liftIO $ readIORef proofCacheRef
          let already = maybe False cacheNegDone (M.lookup key cache) || maybe False (not . null . cacheNeg) (M.lookup key cache)
          if already
            then return $ object ["status" .= ("cached" :: TS.Text)]
            else do
              _ <- liftIO $ scheduleLimited $ do
                let l = LT.take nProof (prover psq)
                m <- LT.uncons l
                case m of
                  Nothing -> atomicModifyIORef' proofCacheRef (\m0 ->
                              let ce = M.findWithDefault emptyCacheEntry key m0
                                  ce' = ce { cacheNegDone = True }
                              in (M.insert key ce' m0, ()))
                  Just (d, _) -> do
                    atomicModifyIORef' proofCacheRef (\m0 ->
                      let ce = M.findWithDefault emptyCacheEntry key m0
                          ce' = ce { cacheNeg = cacheNeg ce ++ [d], cacheNegDone = True }
                      in (M.insert key ce' m0, ()))
                    atomicModifyIORef' currentPSNegRef (\xs -> (xs ++ [d], ()))
                    atomicWriteIORef currentPSDoneNegRef True
              return $ object ["status" .= ("started" :: TS.Text)]
    _ -> return $ object ["status" .= ("no_prover" :: TS.Text)]
-- Export proof search query as binary file
getProofQueryBinR :: Handler TypedContent
getProofQueryBinR = do
  mk <- lookupGetParam "kind"
  q <- case mk of
         Just "pos" -> liftIO $ readIORef currentPSQPosRef
         Just "neg" -> liftIO $ readIORef currentPSQNegRef
         _          -> return Nothing
  case q of
    Nothing -> do
      addHeader "Content-Type" "text/plain; charset=utf-8"
      return $ TypedContent typePlain (toContent ("not ready" :: TS.Text))
    Just psq -> do
      let bs = Store.encode psq
          fname = case mk of
                    Just "neg" -> "proofsearch-query-neg.bin"
                    _          -> "proofsearch-query-pos.bin"
      addHeader "Content-Type" "application/octet-stream"
      addHeader "Content-Disposition" (TS.concat ["attachment; filename=\"", fname, "\""])
      return $ TypedContent "application/octet-stream" (toContent bs)

-- Start a typecheck for a given sentence/node
getInfTypecheckStartR :: Handler Value
getInfTypecheckStartR = do
  mSent <- lookupGetParam "sent"
  mTab  <- lookupGetParam "tab"
  let parseInt :: TS.Text -> Maybe Int
      parseInt = readMaybe . TS.unpack
      sIdx = maybe 1 id (mSent >>= parseInt)
      nIdx = maybe 1 id (mTab  >>= parseInt)
      key = (sIdx, nIdx)
  -- Check and start if needed
  st <- liftIO $ readIORef currentTCStateRef
  sel <- liftIO $ readIORef currentTCSelectionRef
  -- 依存（前文の選択）が満たされているかを確認
  let depsOk = all (\i -> i <= 0 || M.member i sel) [sIdx - 1]
  if not depsOk
    then return $ object ["status" .= ("blocked" :: TS.Text), "reason" .= ("need_previous_selection" :: TS.Text)]
    else case M.lookup key st of
    Just TCInProgress -> return $ object ["status" .= ("in_progress" :: TS.Text)]
    Just (TCDone _)   -> return $ object ["status" .= ("done" :: TS.Text)]
    Just (TCFailed _) -> return $ object ["status" .= ("failed" :: TS.Text)]
    _ -> do
      -- mark in progress
      liftIO $ atomicModifyIORef' currentTCStateRef (\m -> (M.insert key TCInProgress m, ()))
      -- spawn background job
      _ <- liftIO $ forkIO $ do
        mDisc <- readIORef currentDiscourseNodesRef
        mProver <- readIORef currentProverRef
        baseSig <- readIORef currentBaseSignatureRef
        baseCtx <- readIORef currentBaseContextRef
        nmax <- readIORef currentNTypeCheckRef
        verbose <- readIORef currentVerboseRef
        selMap <- readIORef currentTCSelectionRef
        let (!!?) :: [a] -> Int -> Maybe a
            (!!?) xs n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)
        case (mDisc, mProver) of
          (Just disc, Just prover) -> do
            let s0 = sIdx - 1
                n0 = nIdx - 1
            case disc !!? s0 of
              Nothing -> atomicModifyIORef' currentTCStateRef (\m -> (M.insert key (TCFailed "bad sentence") m, ()))
              Just sp -> case snNodes sp !!? n0 of
                Nothing   -> atomicModifyIORef' currentTCStateRef (\m -> (M.insert key (TCFailed "bad node") m, ()))
                Just node -> do
                  let signtr' = CCG.sig node ++ baseSig
                      -- 直前までの選択からコンテキストを構築（選択図の項を連結）
                      ctxPrev = baseCtx ++ [ DTT.trm (Tree.node (selDiagram sel)) | i <- [1..(sIdx-1)], Just sel <- [M.lookup i selMap] ]
                      tcQueryType = UDTT.Judgment signtr' ctxPrev (CCG.sem node) DTT.Type
                      lts = TY.typeCheck prover verbose tcQueryType
                  diags <- LT.toList (LT.take nmax lts)
                  atomicModifyIORef' currentTCStateRef (\m -> (M.insert key (TCDone diags) m, ()))
          _ -> atomicModifyIORef' currentTCStateRef (\m -> (M.insert key (TCFailed "no prover") m, ()))
      return $ object ["status" .= ("started" :: TS.Text)]

-- Get result snippet for a given sentence/node
getInfTypecheckResultR :: Handler Html
getInfTypecheckResultR = do
  mSent <- lookupGetParam "sent"
  mTab  <- lookupGetParam "tab"
  let parseInt :: TS.Text -> Maybe Int
      parseInt = readMaybe . TS.unpack
      sIdx = maybe 1 id (mSent >>= parseInt)
      nIdx = maybe 1 id (mTab  >>= parseInt)
      key = (sIdx, nIdx)
  st <- liftIO $ readIORef currentTCStateRef
  dsp <- liftIO $ readIORef currentDisplaySettingRef
  selMap <- liftIO $ readIORef currentTCSelectionRef
  let selIdx :: Int
      selIdx = case M.lookup sIdx selMap of
                 Just s | selNodeIdx s == nIdx -> selDiagIdx s
                 _ -> 0
  case M.lookup key st of
    Just (TCDone diags) -> do
      let indexed = Prelude.zip ([1..] :: [Int]) diags
      defaultLayout $ do
        if Prelude.null diags
          then [whamlet|<p .error-message>⚠️ Type Check Failed... ⚠️|]
          else [whamlet|
            <div class="tab-tcds-content">
              $forall (idx, d) <- indexed
                <div .tab-tcds-inner :idx == selIdx:.tc-selected data-diag-idx=#{idx} onclick="selectTypecheck(#{sIdx},#{nIdx},#{idx})" title="select this diagram">
                  $if idx == selIdx
                    <div .badge-selected>Selected
                  ^{WE.widgetizeWith dsp d}
          |]
    Just (TCFailed msg) -> defaultLayout $ do
      [whamlet|<div .error-message>TypeCheck failed: #{msg}|]
    _ -> return [shamlet|<div data-tc-loading=1 .span-preview-loading>loading...|]

-- Select a diagram for a sentence (to unlock next sentence)
getInfTypecheckSelectR :: Handler Value
getInfTypecheckSelectR = do
  mSent <- lookupGetParam "sent"
  mTab  <- lookupGetParam "tab"
  mIdx  <- lookupGetParam "idx"
  let parseInt :: TS.Text -> Maybe Int
      parseInt = readMaybe . TS.unpack
      sIdx = maybe 1 id (mSent >>= parseInt)
      nIdx = maybe 1 id (mTab  >>= parseInt)
      dIdx = maybe 1 id (mIdx  >>= parseInt)
  st <- liftIO $ readIORef currentTCStateRef
  case M.lookup (sIdx, nIdx) st of
    Just (TCDone diags) ->
      let i = dIdx - 1
      in if i < 0 || i >= length diags
            then return $ object ["status" .= ("error" :: TS.Text), "reason" .= ("bad_index" :: TS.Text)]
            else do
              let chosen = diags !! i
              -- 1) 選択を保存（この文だけ上書き）
              liftIO $ atomicModifyIORef' currentTCSelectionRef (\m -> (M.insert sIdx (TCSelection nIdx dIdx chosen) m, ()))
              -- 2) 後続文の選択をクリア（整合性のため）
              liftIO $ atomicModifyIORef' currentTCSelectionRef (\m -> (M.filterWithKey (\k _ -> k <= sIdx) m, ()))
              -- 3) 後続文のTypeCheck状態をクリア（再計算させる）
              liftIO $ atomicModifyIORef' currentTCStateRef (\m -> (M.filterWithKey (\(k,_) _ -> k <= sIdx) m, ()))
              return $ object ["status" .= ("ok" :: TS.Text)]
    _ -> return $ object ["status" .= ("error" :: TS.Text), "reason" .= ("no_diagrams" :: TS.Text)]
    
-- HTML snippet: render a single node like Leaf Node layout
getNodeR :: Handler Html
getNodeR = do
  mpr <- liftIO $ readIORef currentParseResultRef
  case mpr of
    Nothing -> return [shamlet|<div class="error-message">ParseResult is not set|]
    Just pr -> do
      let text_sen = case pr of
                        NLI.SentenceAndParseTrees sentence _ -> sentence
                        _ -> T.empty
      mStart <- lookupGetParam "start"
      mEnd <- lookupGetParam "end"
      mSurface <- lookupGetParam "surface"
      mBeam <- lookupGetParam "beam"
      mIndex <- lookupGetParam "index"
      let parseInt :: TS.Text -> Maybe Int
          parseInt = readMaybe . TS.unpack
      let beam :: Int
          beam = case mBeam >>= parseInt of
                   Just b -> b
                   Nothing -> 32
          idx :: Int
          idx = case mIndex >>= parseInt of
                  Just i -> i
                  Nothing -> 0
      chart <- liftIO $ case (mStart >>= parseInt, mEnd >>= parseInt, mSurface) of
                          (Just _, Just _, _) -> L.parse beam text_sen
                          (_, _, Just surf)   -> L.parse beam (T.fromStrict surf)
                          _                   -> L.parse beam text_sen
      let entries = M.toList chart
          normalize :: TS.Text -> TS.Text
          normalize = TS.filter (\c -> not (c `elem` delims))
          delims :: [Char]
          delims = [' ', '　', '。', '、', '，', '．', ',', '.', '!', '！', '?', '？']
      jpOptions <- liftIO PL.defaultJpOptions
      (tokens, _) <- liftIO $ JP.setupLexicon jpOptions text_sen
      let tokensN = map (normalize . T.toStrict) tokens
          nTok = length tokensN
          matchSpan :: TS.Text -> Maybe (Int,Int)
          matchSpan target
            | TS.null target = Nothing
            | otherwise =
                let go s e = if s >= nTok then Nothing
                              else if e > nTok then go (s+1) (s+2)
                              else let catse = TS.concat (take (e-s) (drop s tokensN)) in
                                   if catse == target then Just (s,e) else go s (e+1)
                in go 0 1
          pickByIndex :: Int -> Int -> [((Int,Int), [a])] -> Maybe ((Int,Int), [a])
          pickByIndex i j = find (\((i',j'),_) -> i'==i && j'==j)
      chosen <- case (mStart >>= parseInt, mEnd >>= parseInt, mSurface) of
                  (Just i, Just j, _) -> return $ pickByIndex i j entries
                  (_, _, Just surf) ->
                    case matchSpan (normalize surf) of
                      Just (i,j) -> return $ pickByIndex i j entries
                      Nothing    -> return Nothing
                  _ -> return Nothing
      nodeM <- case chosen of
                 Just (_, ns) -> return $ if null ns then Nothing else (ns !!? idx)
                 Nothing -> do
                   case mSurface of
                     Nothing -> return Nothing
                     Just surf -> do
                       subChart <- liftIO $ L.parse beam (T.fromStrict surf)
                       let prSub = Partial.extractParseResult beam subChart
                       case prSub of
                         Partial.Failed   -> return Nothing
                         Partial.Full ns  -> return $ ns !!? idx
                         Partial.Partial ns -> return $ ns !!? idx
      dsp <- liftIO $ readIORef currentDisplaySettingRef
      let dspCollapsed = dsp { WE.defaultExpandDepth = 0 }
      case nodeM of
        Nothing -> defaultLayout $ do
          [whamlet|<div class="error-message">No node|]
        Just node -> defaultLayout $ do
          WE.widgetizeWith dspCollapsed node
  where
    (!!?) :: [a] -> Int -> Maybe a
    (!!?) xs n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)

-- Text export: UDTT preterm (semantics) as plain text for a selected tab's node
getExportSemTextR :: Handler TypedContent
getExportSemTextR = do
  mpr <- liftIO $ readIORef currentParseResultRef
  case mpr of
    Nothing -> sendResponse (TypedContent "text/plain" (toContent ("ParseResult is not set" :: TS.Text)))
    Just pr -> do
      nodes <- liftIO $ L.parseSentence' pr
      mTab <- lookupGetParam "tab"
      let parseInt :: TS.Text -> Maybe Int
          parseInt = readMaybe . TS.unpack
          tab = maybe 1 id (mTab >>= parseInt)
          idx = tab - 1
      case nodes !!? idx of
        Nothing -> sendResponse (TypedContent "text/plain" (toContent ("Invalid tab index" :: TS.Text)))
        Just node -> do
          let semTerm = CCG.sem node
              txt = T.toStrict (toText semTerm)
          addHeader "Content-Type" "text/plain; charset=utf-8"
          sendResponse (TypedContent "text/plain" (toContent txt))
  where
    (!!?) :: [a] -> Int -> Maybe a
    (!!?) xs n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)

-- Binary export: UDTT preterm (semantics) of a selected tab's node
getExportSemR :: Handler TypedContent
getExportSemR = do
  mpr <- liftIO $ readIORef currentParseResultRef
  case mpr of
    Nothing -> sendResponse (TypedContent "text/plain" (toContent ("ParseResult is not set" :: TS.Text)))
    Just pr -> do
      nodes <- liftIO $ L.parseSentence' pr
      mTab <- lookupGetParam "tab"
      let parseInt :: TS.Text -> Maybe Int
          parseInt = readMaybe . TS.unpack
          tab = maybe 1 id (mTab >>= parseInt)
          idx = tab - 1
      case nodes !!? idx of
        Nothing -> sendResponse (TypedContent "text/plain" (toContent ("Invalid tab index" :: TS.Text)))
        Just node -> do
          let semTerm = CCG.sem node
              bs :: BS.ByteString
              bs = Store.encode semTerm
              fname = TS.pack $ "sem_tab" ++ show tab ++ ".udtt"
          addHeader "Content-Type" "application/octet-stream"
          addHeader "Content-Disposition" (TS.concat ["attachment; filename=\"", fname, "\""])
          sendResponse (TypedContent "application/octet-stream" (toContent bs))
  where
    (!!?) :: [a] -> Int -> Maybe a
    (!!?) xs n = if n < 0 || n >= length xs then Nothing else Just (xs !! n)

-- Binary export: CCG node (syntax tree) of a selected tab (placeholder)
getExportNodeR :: Handler TypedContent
getExportNodeR = do
  addHeader "Content-Type" "text/plain"
  sendResponse (TypedContent "text/plain" (toContent ("Not implemented yet" :: TS.Text)))