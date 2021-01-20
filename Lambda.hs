#!/usr/bin/env runhaskell
module Main where
import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import System.Environment
import Text.ParserCombinators.Parsec hiding(State)

type Ident = String
instance Show (a -> b) where show x = "Builtin"
data Expr = EApp Expr Expr | ELam Ident Expr | EBuilt (Expr -> IO Expr)
    | EInt Int | EId Ident deriving Show
newtype MacroFunc = MacroFunc ([Expr] -> State ParseState Expr)
type ParseState = M.Map Ident (Int, MacroFunc)
type MyParser a = GenParser Char ParseState a


runWithState :: State ParseState a -> MyParser a
runWithState upd = do
    (e, s') <- liftM (runState upd) getState
    setState s'
    return e

-------------------------------------------------------------------
-- XXX in latest Parsec lib, but not in 6.6.1
optionMaybe :: GenParser tok st a -> GenParser tok st (Maybe a)
optionMaybe p       = option Nothing (liftM Just p)
-- XXX

pOptWs :: MyParser String
pOptWs = many (space <|> tab <|> newline)

lexeme :: MyParser a -> MyParser a
lexeme = between pOptWs pOptWs

chtok :: Char -> MyParser Char
chtok ch = lexeme $ char ch

pNum :: MyParser Int
pNum = lexeme $
    ((char '-' >> liftM (negate.read) (many1 digit))
     <|> liftM read (many1 digit))

pIdent :: MyParser Ident
pIdent = lexeme $ many1 (lower <|> upper <|> digit <|> char '_')

pComment :: MyParser ()
pComment = lexeme $ char '#' >> many (satisfy (/= '\n')) >> return ()

pLambda :: MyParser Expr
pLambda = do
    chtok '\\'
    liftM2 ELam pIdent pExpr

pMacro :: MyParser Expr
pMacro = do
    chtok '['
    name <- pIdent
    args <- many pExprBase
    chtok ']'
    mfunc <- liftM (M.lookup name) getState
    let (nargs, MacroFunc f) = 
         maybe (error $ "Undefined macro " ++ name) id mfunc
    when (length args /= nargs) 
         (error $ "wrong number of arguments for " ++ name)
    runWithState (f args)

pParenExpr :: MyParser Expr
pParenExpr = between (chtok '(') (chtok ')') pExpr

pExprBase :: MyParser Expr
pExprBase =
    (pComment >> pExprBase)
    <|> liftM EInt pNum
    <|> liftM EId pIdent 
    <|> pParenExpr
    <|> pMacro
    <|> pLambda

pExpr :: MyParser Expr
pExpr = liftM (foldl1 EApp) (many pExprBase)

pWholeExpr :: MyParser (Expr, ParseState)
pWholeExpr = do
    e <- pExpr
    eof
    s <- getState
    return (e,s)

parseFile' :: FilePath -> ParseState -> IO (Expr, ParseState)
parseFile' fn st = do
    c <- readFile fn
    case (runParser pWholeExpr st fn c) of
        Left err -> error ("Parse error at " ++ show err)
        Right x -> return x

parseFile :: Bool -> FilePath -> IO Expr
parseFile usePrelude fn = do
    let initState = M.singleton "defmacro" (3, MacroFunc defMacro)
    s <- if usePrelude
            then liftM snd $ parseFile' "prelude.lam" initState
            else return initState
    liftM fst $ parseFile' fn s

-------------------------------------------------------------------
expandMacro :: Expr -> [Expr] -> State ParseState Expr
expandMacro body args = return $
    foldr (uncurry replaceM) body (zip (macroArgs (length args)) args)

genArgs :: String -> Int -> [Ident]
genArgs pref n = [pref ++ "arg" ++ show m | m <- [0..n-1]]
macroArgs :: Int -> [String]
macroArgs = genArgs "$$"

renameArgs :: Int -> Expr -> Expr
renameArgs nargs body = foldr (\(a, a') -> replaceM a (EId a')) body 
    (zip (genArgs "" nargs) (macroArgs nargs))

defMacro :: [Expr] -> State ParseState Expr
defMacro [(EId name), (EInt nargs), body] = do
    let body' = renameArgs nargs body
    modify (M.insert name (nargs, MacroFunc (expandMacro body')))
    return (ELam "$$x" (EId "$$x"))


-------------------------------------------------------------------
binFunc :: (Expr -> Expr -> IO Expr) -> Expr
binFunc f = EBuilt $ \e1 -> return $ EBuilt $ \e2 -> f e1 e2

withInts :: (a -> Expr) -> (Int -> Int -> a) -> Expr
withInts makeRes f = binFunc (\a b -> do
    ia <- getInt a
    ib <- getInt b
    return $ makeRes $ f ia ib)

getInt :: Expr -> IO Int
getInt e = liftM getInt' (eval e)
    where getInt' (EInt x) = x
          getInt' x = error $ "expected integer, not " ++ show x

intFunc :: (Int -> Int -> Int) -> Expr
intFunc = withInts EInt
boolFunc :: (Int -> Int -> Bool) -> Expr
boolFunc = withInts makeBool
makeBool :: Bool -> Expr
makeBool False = ELam "x" (ELam "y" (EId "x"))
makeBool True  = ELam "x" (ELam "y" (EId "y"))

seqFunc :: Expr -> Expr -> IO Expr
seqFunc e1 e2 = do
    ee1 <- eval e1
    return $ seq ee1 e2
traceFunc :: Expr -> Expr -> IO Expr
traceFunc (EId id) e = do
    e' <- eval e
    putStrLn (id ++ " " ++ show e')
    return e'

builtins :: [(String, Expr)]
builtins = [("add",   intFunc  (+)),
            ("sub",   intFunc  (-)),
            ("mul",   intFunc  (*)),
            ("div",   intFunc  div),
            ("mod",   intFunc  mod),
            ("eq",    boolFunc (==)),
            ("lt",    boolFunc (<)),
            ("seq",   binFunc  seqFunc),
            ("trace", binFunc  traceFunc)]

-------------------------------------------------------------------
replace :: Ident -> Expr -> Expr -> Expr
replace var val e = replace' e
    where r = replace var val
          replace' (EApp a b)                 = EApp (r a) (r b)
          replace' (ELam id body) | id /= var = ELam id (r body)
          replace' (EId id)       | id == var = val
          replace' x                          = x

replaceM :: Ident -> Expr -> Expr -> Expr
replaceM var val e = replace' e
    where r = replaceM var val
          replaceId id' = case val of
             (EId id) -> id
             x        -> error $ "Can't replace lambda variable with " ++ show x
          replace' (EApp a b)                 = EApp (r a) (r b)
          replace' (ELam id body) | id /= var = ELam id (r body)
                                  | otherwise = ELam (replaceId id) (r body)
          replace' (EId id)       | id == var = val
          replace' x                          = x


eval :: Expr -> IO Expr
eval e@(EApp e1 e2) = do
    ee1 <- eval e1
    case ee1 of
      (ELam id body) -> eval $ replace id e2 body
      (EBuilt f)     -> eval e2 >>= f
      _              -> return e
eval e@(EId x) = return $ maybe e id (lookup x builtins)
eval x = return $ x


-------------------------------------------------------------------
main = do 
    args <- getArgs
    let progFn = if length args == 1
                    then head args
                    else error "Usage:  prog filename"
    prog <- parseFile True progFn
    eval prog >>= print

