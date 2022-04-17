module Lib
  ( someFunc,
  )
where

import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Text.Parsec
import qualified Text.Parsec.String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data KSDecls = KSDecls {lastFnID :: Integer, clsDecls :: [String], clsIds :: [String], fnMap :: Map Integer String, oldFns :: Map Integer (Maybe Integer), currFns :: Map String Integer}

-- mangle path = let y = map (\x -> show (length x) ++ x) path in "ksU" ++ show (length y) ++ (y >>= id)

ksState =
  unlines
    [ "data Fir m t = Monad (m t) | Fir t",
      "instance (Monad m) => Monad (Fir m) where",
      "  return x = Monad $ return x",
      "  (Monad a) >>= b = flat (a >>= (return . b))",
      "  (Fir v) >>= _ = v",
      "flat (Monad m) = Monad $ m >>= id",
      "flat (Fir x) = Fir x",
      "getRetDiscardingFir (Monad m) = m",
      "getRetDiscardingFir (Fir v) = return v",
      "instance MonadTrans Fir where",
      "  lift = Monad",
      "data Fn m = Fn (Object -> Fir m Object) (Maybe Integer)",
      "ksTrapCall fn val = ksUtoFn fn >>= (\\(FnObj (Fn fn ident)) -> getRetDiscardingFir (fn val) >>= (\\case of",
      "  Panic -> after ident >>= (\\i -> case i of",
      "    Just f2 -> ksTrapCall f2 val",
      "    Nothing -> return $ Panic)",
      "  Bail x -> Fir x",
      "  _@x -> return x))",
      "ksUToFn (FnObj x) = FnObj x",
      "after fn = fn >>= (\\x -> after_ fn)",
      "ksLam l = Fn l Nothing",
      "ksUinvalid = Fir $ Panic",
      "ksUbail = Fir . Bail . return",
      "ksUinner x = fmap ksInner x"
    ]

ksCreateClass name = do
  state <- getState
  putState (state {clsDecls = clsDecls state ++ ["KsU" ++ name ++ " (Object)"], clsIds = clsIds state ++ ["kstyIdIs (KsU" ++ name ++ " _) (KsU" ++ name ++ " _) a b = a", 
  "ksInner (KsU" ++ name ++ " x) = x","ksU" ++ name ++ " x = fmap KsU" ++ name ++ " x"]})
  return ""

ksRuntime = unlines ["to must x y do tyIdIs x y (x) (kinvalid)"]

ksCreateFn name param body = do
  state <- getState
  let h = name ++ "_" ++ show (hash body)
  let tgt = unlines ["ksL" ++ h ++ " " ++ param ++ " = " ++ body, "ksU" ++ h ++ " = (ksL" ++ name ++ "," ++ show (lastFnID state) ++ ")"]
  putState (state {lastFnID = lastFnID state + 1, fnMap = Map.insert (lastFnID state) h (fnMap state), oldFns = Map.insert (lastFnID state) (Map.lookup name (currFns state)) (oldFns state), currFns = Map.insert name (lastFnID state) $ currFns state})
  return tgt

ksRenderDecls decls =
  unlines
    ( [ "data Object = FnObj (Fn Identity) | Panic | Bail Object" ++ (clsDecls decls >>= (\x -> " | " ++ x)),
        "ksUtyIdIs = ksLam (\\a -> return $ ksLam (\\b -> return $ ksLam (\\c -> return $ ksLam (\\d -> return $ ksLam (\\e -> ksTrapCall (kstyIdIs a b c d) e))))",
        "kstyIdIs (FnObj _) (FnObj _) a b = a"
      ]
        ++ clsIds decls
        ++ (map (\(x, y) -> "after_ " ++ (show x) ++ " = return $ Just (" ++ y ++ ", " ++ show (fromJust $ (oldFns decls) Map.! x) ++ ")") $ filter (\(x, y) -> Map.member x (oldFns decls) && isJust ((oldFns decls) Map.! x)) $ Map.toList $ fnMap decls)
        ++ [ "kstyIdIs _ _ a b = b",
             "after_ _ = return $ Nothing"
           ]
    )
ksRender = do
    d <- getState
    return $ ksRenderDecls d
ksCompileTo = try (string "to" >> spaces >> many1 alphaNum >>= (\name -> spaces >> many1 alphaNum >>= (\param -> spaces >> between (string "do") (string "end") ksCompileExpr >>= (\e -> ksCreateFn name param e))))
ksCompileClass :: Parsec String KSDecls String
ksCompileClass = try (string "class" >> spaces >> many1 alphaNum >>= (ksCreateClass))
ksCompileStmt = ksCompileTo <|> ksCompileClass
ksCompileP = many (ksCompileStmt) >>= (\s -> ksRender >>= (\r -> return $ unlines [ksState, unlines s, r]))

ksCompileExpr :: Parsec String KSDecls String
ksCompileExpr =
  ( try (fmap (\x -> "ksU" ++ x) $ many1 alphaNum)
      <|> try (char '\\' >> many1 alphaNum >>= (\ident -> spaces >> string "=>" >> spaces >> ksCompileExpr >>= (\body -> return $ "ksLam ((\\ksU" ++ ident ++ " -> " ++ body ++ ") . return)")))
      <|> try (between (char '(') (char ')') ksCompileExpr)
      <|> try (ksCompileExpr >>= (\x -> spaces >> ksCompileExpr >>= (\y -> return $ "(ksTrapCall " ++ x ++ " " ++ y ++ ")")))
  )