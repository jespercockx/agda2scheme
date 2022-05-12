module Agda.Compiler.ToScheme where

import Prelude hiding ( null , empty )

import Agda.Compiler.Common
import Agda.Compiler.Erase ( runE , erasable , getFunInfo )
import Agda.Compiler.ToTreeless
import Agda.Compiler.Treeless.EliminateLiteralPatterns
import Agda.Compiler.Treeless.Erase
import Agda.Compiler.Treeless.GuardsToPrims

import Agda.Syntax.Abstract.Name
import Agda.Syntax.Common
import Agda.Syntax.Internal as I
import Agda.Syntax.Literal
import Agda.Syntax.Treeless

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Primitive.Base

import Agda.Utils.Impossible
import Agda.Utils.Lens
import Agda.Utils.List
import Agda.Utils.Maybe
import Agda.Utils.Monad
import Agda.Utils.Null
import qualified Agda.Utils.Pretty as P
import Agda.Utils.Singleton

import Control.Arrow ( first , second )
import Control.DeepSeq ( NFData )

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Char
import Data.SCargot.Repr
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics ( Generic )

type SchAtom = Text
type SchForm = RichSExpr SchAtom

schDefine :: SchAtom -> [SchAtom] -> SchForm -> SchForm
schDefine f xs body = RSList
  ["define", RSList (map RSAtom (f:xs)), body]

schError :: Text -> SchForm
schError msg = RSList
  [ "begin"
  , RSList ["display", RSAtom ("\"" <> msg <> "\\n\"")]
  , RSList ["exit", "1"]
  ]

schAxiom :: SchAtom -> SchForm
schAxiom f = schDefine f [] $ schError $ "encountered axiom: " <> f

schLambda :: [SchAtom] -> SchForm -> SchForm
schLambda args body = RSList
  [ RSAtom "lambda"
  , RSList $ map RSAtom args
  , body
  ]

-- Bind each argument individually instead of all at once.
schLambdas :: [SchAtom] -> SchForm -> SchForm
schLambdas args body = foldr (schLambda . singleton) body args

-- Apply to each argument individually instead of all at once.
schApps :: SchForm -> [SchForm] -> SchForm
schApps f args = foldl (\x y -> RSList [x,y]) f args

schLet :: [(SchAtom,SchForm)] -> SchForm -> SchForm
schLet binds body = RSList
  [ RSAtom "let"
  , RSList $ map (\(x,v) -> RSList [RSAtom x,v]) binds
  , body
  ]

schConAtom :: SchAtom -> SchAtom
schConAtom x = T.singleton '\'' <> x

schCase :: SchForm -> [SchForm] -> Maybe SchForm -> SchForm
schCase x cases maybeFallback = RSList $
  [ RSAtom "record-case"
  , x
  ] ++ cases ++
  [ RSList [ RSAtom "else" , fallback ] | fallback <- maybeToList maybeFallback
  ]

schUnit :: SchForm
schUnit = RSList [RSAtom "list"]

schInt :: Int -> SchForm
schInt i = RSAtom $ T.pack $ show i

schDelay :: SchForm -> SchForm
schDelay x
  | RSList [RSAtom "force", y] <- x = y
  | otherwise                       = RSList [RSAtom "delay", x]

schForce :: SchForm -> SchForm
schForce x
  | RSList [RSAtom "delay", y] <- x = y
  | otherwise                       = RSList [RSAtom "force", x]

schLookupList :: SchForm -> SchForm -> SchForm
schLookupList xs k = RSList [RSAtom "list-ref", xs, k]

dropArgs :: [Bool] -> [a] -> [a]
dropArgs bs xs = map snd $ filter (not . fst) $ zip bs xs

-- Apply a function symbol of given arity and erasure info to a list of arguments,
-- inserting lambdas or applications where needed to match the
-- symbol's arity.
schApp :: Int -> [Bool] -> SchForm -> [SchForm] -> ToSchemeM SchForm
schApp n bs f args = do
  let m = n - length args
  if | m >= 0    -> withFreshVars m $ \vars -> do
        let args2 = map RSAtom vars
        return $ schLambdas vars $ RSList $ f : dropArgs bs (args ++ args2)
     | otherwise -> do
         let (args1,args2) = splitAt n args
         return $ schApps (RSList (f : dropArgs bs args1)) args2

schConApp :: SchAtom -> Int -> Bool -> [Bool] -> [SchForm] -> ToSchemeM SchForm
schConApp c n b bs args = do
  let tag = if b then id else (RSAtom (schConAtom c) :)
      m = n - length args
  unless (m >= 0) __IMPOSSIBLE__
  force <- makeForce
  withFreshVars m $ \vars -> do
    let args2 = map (force . RSAtom) vars
    return $ schLambdas vars $
      RSList $ RSAtom "list" : tag (dropArgs bs (args ++ args2))

schOp :: Int -> Text -> [SchForm] -> ToSchemeM SchForm
schOp n op args = schApp n (replicate n False) (RSAtom op) args

schPrimOp :: TPrim -> [SchForm] -> ToSchemeM SchForm
schPrimOp p args = case p of
  PAdd  -> schOp 2 "+"   args
  PSub  -> schOp 2 "-"   args
  PMul  -> schOp 2 "*"   args
  PQuot -> schOp 2 "div" args
  PRem  -> schOp 2 "mod" args
  PIf   -> schOp 3 "if"  args
  PEqI  -> schOp 2 "="   args
  PGeq  -> schOp 2 ">="  args
  PLt   -> schOp 2 "<"   args
  PSeq  -> schOp 2 "seq" args
  _     -> fail $ "not yet supported: primitive " ++ show p

schPreamble :: ToSchemeM [SchForm]
schPreamble = do
  force <- makeForce
  strat <- getEvaluationStrategy
  return
    [ RSList
      [ RSAtom "import"
      , RSList [ RSAtom "only" , RSList [RSAtom "chezscheme"] , RSAtom "record-case" ]
      ]
      -- TODO: put this in a separate file and import it here
    , schDefine "monus" ["x","y"] $
        RSList [RSAtom "max", RSAtom "0", RSList [RSAtom "-", force (RSAtom "x"), force (RSAtom "y")]]
    , schDefine "seq" ["x","y"] $ case strat of
        EagerEvaluation -> RSAtom "y"
        LazyEvaluation  -> RSList [RSAtom "begin", force (RSAtom "x"), RSAtom "y"]
    ]

deriving instance Generic EvaluationStrategy
deriving instance NFData  EvaluationStrategy

data SchOptions = SchOptions
  { schEvaluation :: EvaluationStrategy
  }
  deriving (Generic, NFData)

data ToSchemeEnv = ToSchemeEnv
  { toSchemeOptions :: SchOptions
  , toSchemeVars    :: [SchForm]
  }

initToSchemeEnv :: SchOptions -> ToSchemeEnv
initToSchemeEnv opts = ToSchemeEnv opts []

addBinding :: SchForm -> ToSchemeEnv -> ToSchemeEnv
addBinding x env = env { toSchemeVars = x : toSchemeVars env }

data ToSchemeDef = ToSchemeDef SchAtom Int [Bool]       -- Scheme name + arity + erased args

data ToSchemeCon = ToSchemeCon SchAtom Int Bool [Bool]  -- Scheme name + arity + erased tag + erased args

data ToSchemeState = ToSchemeState
  { toSchemeFresh     :: [SchAtom]                 -- Used for locally bound named variables
  , toSchemeDefs      :: Map QName ToSchemeDef
  , toSchemeCons      :: Map QName ToSchemeCon
  , toSchemeUsedNames :: Set SchAtom               -- Names that are already in use (both variables and definitions)
  }

-- This is an infinite supply of variable names
-- a, b, c, ..., z, a1, b1, ..., z1, a2, b2, ...
-- We never reuse variable names to make the code easier to
-- understand.
freshVars :: [SchAtom]
freshVars = concat [ map (<> i) xs | i <- "":(map (T.pack . show) [1..]) ]
  where
    xs = map T.singleton $ ['a'..'z']

-- These are names that should not be used by the code we generate
reservedNames :: Set SchAtom
reservedNames = Set.fromList $ map T.pack
  [ "define" , "lambda", "let", "let*", "letrec", "letrec*"
  , "let-values", "let*-values", "case"
  , "record-case", "else"
  , "let-syntax", "letrec-syntax"
  , "define-syntax", "syntax-rules"
  , "#t" , "#f" , "if" , "=", "eqv?"
  , "+", "-", "*", "/"
  , "list", "car", "cdr"
  , "vector-ref", "string-ref"
  , "begin", "display", "put-bytes", "exit"
  , "library", "import", "export", "only"
  , "force", "delay"
  , "call-with-values", "call-with-current-continuation"
  , "add", "sub", "mul", "quot", "rem"
  , "iff", "eq", "monus", "seq"
  -- TODO: add more
  ]

initToSchemeState :: ToSchemeState
initToSchemeState = ToSchemeState
  { toSchemeFresh     = freshVars
  , toSchemeDefs      = Map.empty
  , toSchemeCons      = Map.empty
  , toSchemeUsedNames = reservedNames
  }

type ToSchemeM a = StateT ToSchemeState (ReaderT ToSchemeEnv TCM) a

runToSchemeM :: SchOptions -> ToSchemeM a -> TCM a
runToSchemeM opts =
    (`runReaderT` initToSchemeEnv opts)
  . (`evalStateT` initToSchemeState)

freshSchAtom :: ToSchemeM SchAtom
freshSchAtom = do
  names <- gets toSchemeFresh
  case names of
    [] -> fail "No more variables!"
    (x:names') -> do
      modify $ \st -> st { toSchemeFresh = names' }
      ifM (isNameUsed x) freshSchAtom $ {-otherwise-} do
        setNameUsed x
        return x

getEvaluationStrategy :: ToSchemeM EvaluationStrategy
getEvaluationStrategy = reader $ schEvaluation . toSchemeOptions

makeDelay :: ToSchemeM (SchForm -> SchForm)
makeDelay = delayIfLazy <$> getEvaluationStrategy

makeForce :: ToSchemeM (SchForm -> SchForm)
makeForce = forceIfLazy <$> getEvaluationStrategy

delayIfLazy :: EvaluationStrategy -> SchForm -> SchForm
delayIfLazy strat = case strat of
  EagerEvaluation -> id
  LazyEvaluation  -> schDelay

forceIfLazy :: EvaluationStrategy -> SchForm -> SchForm
forceIfLazy strat = case strat of
  EagerEvaluation -> id
  LazyEvaluation  -> schForce

getVar :: Int -> ToSchemeM SchForm
getVar i = reader $ (!! i) . toSchemeVars

withFreshVar :: (SchAtom -> ToSchemeM a) -> ToSchemeM a
withFreshVar f = do
  strat <- getEvaluationStrategy
  withFreshVar' strat f

withFreshVar' :: EvaluationStrategy -> (SchAtom -> ToSchemeM a) -> ToSchemeM a
withFreshVar' strat f = do
  x <- freshSchAtom
  local (addBinding $ forceIfLazy strat $ RSAtom x) $ f x

withFreshVars :: Int -> ([SchAtom] -> ToSchemeM a) -> ToSchemeM a
withFreshVars i f = do
  strat <- getEvaluationStrategy
  withFreshVars' strat i f

withFreshVars' :: EvaluationStrategy -> Int -> ([SchAtom] -> ToSchemeM a) -> ToSchemeM a
withFreshVars' strat i f
  | i <= 0    = f []
  | otherwise = withFreshVar' strat $ \x -> withFreshVars' strat (i-1) (f . (x:))

lookupSchemeDef :: QName -> ToSchemeM ToSchemeDef
lookupSchemeDef n = do
  r <- Map.lookup n <$> gets toSchemeDefs
  case r of
    Nothing -> fail $ "unbound name " <> show (P.pretty n)
    Just a  -> return a

lookupSchemeCon :: QName -> ToSchemeM ToSchemeCon
lookupSchemeCon n = do
  r <- Map.lookup n <$> gets toSchemeCons
  case r of
    Nothing -> fail $ "unbound name " <> show (P.pretty n)
    Just a  -> return a

setSchemeDef :: QName -> ToSchemeDef -> ToSchemeM ()
setSchemeDef n def = do
  modify $ \s -> s { toSchemeDefs = Map.insert n def (toSchemeDefs s) }

setSchemeCon :: QName -> ToSchemeCon -> ToSchemeM ()
setSchemeCon n con = do
  modify $ \s -> s { toSchemeCons = Map.insert n con (toSchemeCons s) }

newSchemeDef :: QName -> Int -> [Bool] -> ToSchemeM SchAtom
newSchemeDef n i bs = do
  a <- makeSchemeName n
  setSchemeDef n (ToSchemeDef a i bs)
  setNameUsed a
  return a

newSchemeCon :: QName -> Int -> Bool -> [Bool] -> ToSchemeM SchAtom
newSchemeCon n i b bs = do
  a <- makeSchemeName n
  setSchemeCon n (ToSchemeCon a i b bs)
  setNameUsed a
  return a

isNameUsed :: SchAtom -> ToSchemeM Bool
isNameUsed x = Set.member x <$> gets toSchemeUsedNames

setNameUsed :: SchAtom -> ToSchemeM ()
setNameUsed x = modify $ \s ->
  s { toSchemeUsedNames = Set.insert x (toSchemeUsedNames s) }

-- Extended alphabetic characters that are allowed to appear in
-- a Scheme identifier
schemeExtendedAlphaChars :: Set Char
schemeExtendedAlphaChars = Set.fromList
  [ '!' , '$' , '%' , '&' , '*' , '+' , '-' , '.' , '/' , ':' , '<' , '=' , '>'
  , '?' , '@' , '^' , '_' , '~'
  ]

-- Categories of unicode characters that are allowed to appear in
-- a Scheme identifier
schemeAllowedUnicodeCats :: Set GeneralCategory
schemeAllowedUnicodeCats = Set.fromList
  [ UppercaseLetter , LowercaseLetter , TitlecaseLetter , ModifierLetter
  , OtherLetter , NonSpacingMark , SpacingCombiningMark , EnclosingMark
  , DecimalNumber , LetterNumber , OtherNumber , ConnectorPunctuation
  , DashPunctuation , OtherPunctuation , CurrencySymbol , MathSymbol
  , ModifierSymbol , OtherSymbol , PrivateUse
  ]

-- True if the character is allowed to be used in a Scheme identifier
isValidSchemeChar :: Char -> Bool
isValidSchemeChar x
  | isAscii x = isAlphaNum x || x `Set.member` schemeExtendedAlphaChars
  | otherwise = generalCategory x `Set.member` schemeAllowedUnicodeCats

-- Creates a valid Scheme name from a (qualified) Agda name.
-- Precondition: the given name is not already in toSchemeDefs.
makeSchemeName :: QName -> ToSchemeM SchAtom
makeSchemeName n = go $ fixName $ P.prettyShow $ qnameName n
  where
    nextName = ('z':) -- TODO: do something smarter

    go s     = ifM (isNameUsed $ T.pack s) (go $ nextName s) (return $ T.pack s)

    fixName s =
      let s' = concat (map fixChar s) in
      if | isNumber (head s') -> "z" ++ s'
         | otherwise          -> s'

    fixChar c
      | isValidSchemeChar c = [c]
      | otherwise           = "\\x" ++ toHex (ord c) ++ ";"

    toHex 0 = ""
    toHex i = toHex (i `div` 16) ++ [fourBitsToChar (i `mod` 16)]

fourBitsToChar :: Int -> Char
fourBitsToChar i = "0123456789ABCDEF" !! i
{-# INLINE fourBitsToChar #-}

class ToScheme a b | a -> b where
  toScheme :: a -> ToSchemeM b

-- We first convert all definitions to treeless and calculate their
-- arity and erasure info, before doing the actual translation to Scheme.
defToTreeless :: Definition -> ToSchemeM (Maybe (Int, [Bool], SchAtom, TTerm))
defToTreeless def
  | defNoCompilation def ||
    not (usableModality $ getModality def) = return Nothing
  | otherwise = do
    let f = defName def
    reportSDoc "toScheme" 5 $ "Compiling definition:" <> prettyTCM f
    case theDef def of
      Axiom{} -> do
        f' <- newSchemeDef f 0 []
        return Nothing
      GeneralizableVar{} -> return Nothing
      d@Function{} | d ^. funInline -> return Nothing
      Function{} -> do
        strat <- getEvaluationStrategy
        maybeCompiled <- liftTCM $ toTreeless strat f
        case maybeCompiled of
          Just body -> do
            let (n, body') = lambdaView body
            er <- erasureInfo f
            case er of
              Nothing -> return Nothing
              Just bs -> do
                reportSDoc "toScheme" 15 $ "Erasure info: " <> text (show bs)
                unless (length bs >= n) __IMPOSSIBLE__
                f' <- newSchemeDef f n (take n bs)
                return $ Just (n, bs, f', body')
          Nothing -> return Nothing
      Primitive{} -> do
        f' <- newSchemeDef f 0 []
        return Nothing -- TODO!
      PrimitiveSort{} -> return Nothing
      Datatype{ dataCons = cs } -> do
        let eraseTag = length cs == 1
        forM_ cs $ \c -> do
          cdef <- theDef <$> getConstInfo c
          case cdef of
            Constructor{ conSrcCon = chead, conArity = nargs } ->
              processCon chead nargs eraseTag
            _ -> __IMPOSSIBLE__
        return Nothing
      Record{ recConHead = chead, recFields = fs } -> do
        processCon chead (length fs) True
        return Nothing
      Constructor{} -> return Nothing
      AbstractDefn{} -> __IMPOSSIBLE__
      DataOrRecSig{} -> __IMPOSSIBLE__
  where
    processCon :: ConHead -> Int -> Bool -> ToSchemeM ()
    processCon chead nargs b = do
      er <- erasureInfo (conName chead)
      whenJust er $ \bs -> do
        reportSDoc "toScheme" 15 $ "Erasure info: " <> text (show bs)
        void $ newSchemeCon (conName chead) nargs b bs


lambdaView :: TTerm -> (Int, TTerm)
lambdaView v = case v of
  TLam    w -> first (1+) $ lambdaView w
  TCoerce w -> lambdaView w
  _         -> (0, v)

-- `Just bs` means that the arguments for which the corresponding
-- position in `bs` is True can be erased
-- `Nothing` means that the entire function can be erased.
erasureInfo :: QName -> ToSchemeM (Maybe [Bool])
erasureInfo f = liftTCM $ runE $ do
  (bs, b) <- getFunInfo f
  if erasable b
    then return Nothing
    else return (Just $ map erasable bs)

instance ToScheme (Int, [Bool], SchAtom, TTerm) SchForm where
  toScheme (n, bs, f, body) =
    withFreshVars n $ \xs ->
      schDefine f (dropArgs bs xs) <$> toScheme body

instance ToScheme TTerm SchForm where
  toScheme v = do
    v <- liftTCM $ eliminateLiteralPatterns (convertGuards v)
    toScheme $ tAppView v

instance ToScheme (TTerm, [TTerm]) SchForm where
  toScheme (TCoerce w, args) = toScheme (w, args)
  toScheme (TApp w args1, args2) = toScheme (w, args1 ++ args2)
  toScheme (w, args) = do
    delay <- makeDelay
    args <- traverse toScheme args
    let lazyArgs = map delay args
    case w of
      TVar i -> do
        x <- getVar i
        return $ schApps x lazyArgs
      TPrim p -> schPrimOp p args
      TDef d -> do
        special <- isSpecialDefinition d
        case special of
          Nothing -> do
            ToSchemeDef d' i bs <- lookupSchemeDef d
            schApp i bs (RSAtom d') lazyArgs
          Just (i, v) -> schApp i (replicate i False) v lazyArgs
      TLam v -> withFreshVar $ \x -> do
        unless (null args) __IMPOSSIBLE__
        body <- toScheme v
        return $ schLambda [x] body
      TLit l -> do
        unless (null args) __IMPOSSIBLE__
        toScheme l
      TCon c -> do
        special <- isSpecialConstructor c
        case special of
          Nothing -> do
            ToSchemeCon c' i b bs <- lookupSchemeCon c
            schConApp c' i b bs args
          Just v -> do
            unless (null args) __IMPOSSIBLE__
            return v
      TLet u v -> do
        unless (null args) __IMPOSSIBLE__
        delay <- makeDelay
        expr <- delay <$> toScheme u
        withFreshVar $ \x -> do
          body <- toScheme v
          return $ schLet [(x,expr)] body
      TCase i info v bs -> do
        unless (null args) __IMPOSSIBLE__
        x <- getVar i
        special <- isSpecialCase info
        case special of
          Nothing | [TACon c nargs v] <- bs -> do
            withFreshVars' EagerEvaluation nargs $ \xs -> do
              ToSchemeCon c' i b bs <- lookupSchemeCon c
              let mkProj i = schLookupList x (schInt $ if b then i else i+1)
                  binds    = zip (dropArgs bs xs) (map mkProj [0..])
              body <- toScheme v
              return $ schLet binds body
          Nothing -> do
            cases <- traverse toScheme bs
            fallback <- if isUnreachable v
                        then return Nothing
                        else Just <$> toScheme v
            return $ schCase x cases fallback
          Just BoolCase -> case bs of
            [] -> __IMPOSSIBLE__
            (TACon c1 _ v1 : bs') -> do
              Con trueC  _ _ <- primTrue
              Con falseC _ _ <- primFalse
              v1' <- toScheme v1
              v2' <- case bs' of
                []                 -> toScheme v
                (TACon _ _ v2 : _) -> toScheme v2
                _                  -> __IMPOSSIBLE__
              let (thenBranch,elseBranch)
                    | c1 == conName trueC  = (v1',v2')
                    | c1 == conName falseC = (v2',v1')
                    | otherwise            = __IMPOSSIBLE__
              return $ RSList [RSAtom "if", x, thenBranch, elseBranch]
      TUnit -> do
        unless (null args) __IMPOSSIBLE__
        return schUnit
      TSort -> do
        unless (null args) __IMPOSSIBLE__
        return schUnit
      TErased -> return schUnit
      TError err -> toScheme err

    where
      isUnreachable v = v == TError TUnreachable

instance ToScheme Literal SchForm where
  toScheme lit = case lit of
    LitNat    x -> return $ RSAtom (T.pack (show x))
    LitWord64 x -> return $ schError "not yet supported: Word64 literals"
    LitFloat  x -> return $ schError "not yet supported: Float literals"
    LitString x -> return $ schError "not yet supported: String literals"
    LitChar   x -> return $ schError "not yet supported: Char literals"
    LitQName  x -> return $ schError "not yet supported: QName literals"
    LitMeta p x -> return $ schError "not yet supported: Meta literals"

instance ToScheme TAlt SchForm where
  toScheme alt = case alt of
    TACon c nargs v -> withFreshVars' EagerEvaluation nargs $ \xs -> do
      ToSchemeCon c' i b bs <- lookupSchemeCon c
      when b __IMPOSSIBLE__
      body <- toScheme v
      return $ RSList [RSList [RSAtom c'], RSList (dropArgs bs (map RSAtom xs)), body]

    TAGuard{} -> __IMPOSSIBLE__
    TALit{}   -> __IMPOSSIBLE__

instance ToScheme TError SchForm where
  toScheme err = case err of
    TUnreachable -> return $ schError "Panic!"
    TMeta s      -> return $ schError $ "encountered unsolved meta: " <> T.pack s

isSpecialConstructor :: QName -> ToSchemeM (Maybe SchForm)
isSpecialConstructor c = do
  let getConName (Just (Con c _ _)) = Just (conName c)
      getConName _ = Nothing
  mTrue <- getConName <$> getBuiltin' builtinTrue
  mFalse <- getConName <$> getBuiltin' builtinFalse
  if | Just c == mTrue  -> return $ Just (RSAtom "#t")
     | Just c == mFalse -> return $ Just (RSAtom "#f")
     | otherwise        -> return Nothing

isSpecialDefinition :: QName -> ToSchemeM (Maybe (Int, SchForm))
isSpecialDefinition f = do
  minusDef <- getBuiltinName builtinNatMinus
  if | Just f == minusDef -> return $ Just (2 , RSAtom "monus")
     | otherwise          -> return Nothing

-- Some kinds of case statements are treated in a special way.
-- Currently, matches on Bool are translated to an `if` statement.
data SpecialCase = BoolCase

isSpecialCase :: CaseInfo -> ToSchemeM (Maybe SpecialCase)
isSpecialCase (CaseInfo lazy (CTData q cty)) = do
  mBool <- getBuiltin' builtinBool
  if mBool == Just (Def cty [])
    then return (Just BoolCase)
    else return Nothing
specialCase _ = return Nothing
