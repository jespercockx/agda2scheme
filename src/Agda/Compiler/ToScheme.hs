module Agda.Compiler.ToScheme where

import Prelude hiding ( null , empty )

import Agda.Compiler.Common
import Agda.Compiler.ToTreeless

import Agda.Syntax.Abstract.Name
import Agda.Syntax.Common
import Agda.Syntax.Internal ( conName )
import Agda.Syntax.Literal
import Agda.Syntax.Treeless

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty

import Agda.Utils.Impossible
import Agda.Utils.Lens
import Agda.Utils.List
import Agda.Utils.Maybe
import Agda.Utils.Monad
import Agda.Utils.Null
import Agda.Utils.Pretty
import Agda.Utils.Singleton

import Control.DeepSeq ( NFData )

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

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

schName :: QName -> SchAtom
schName n = T.pack $ prettyShow $ qnameName n

schDefine :: QName -> SchForm -> SchForm
schDefine f body = RSList
  ["define", RSList [RSAtom (schName f)], body]

schError :: Text -> SchForm
schError msg = RSList
  [ "begin"
  , RSList ["display", RSAtom ("\"" <> msg <> "\\n\"")]
  , RSList ["exit", "1"]
  ]

schAxiom :: QName -> SchForm
schAxiom f = schDefine f $ schError $ "encountered axiom: " <> schName f

schLambda :: [SchAtom] -> SchForm -> SchForm
schLambda args body = RSList
  [ RSAtom "lambda"
  , RSList $ map RSAtom args
  , body
  ]

-- Bind each argument individually instead of all at once.
schLambdas :: [SchAtom] -> SchForm -> SchForm
schLambdas args body = foldr (schLambda . singleton) body args

schApp :: SchForm -> [SchForm] -> SchForm
schApp f vs = RSList (f : vs)

-- Apply to each argument individually instead of all at once.
schApps :: SchForm -> [SchForm] -> SchForm
schApps f args = foldl (\x y -> schApp x [y]) f args

schLet :: [(SchAtom,SchForm)] -> SchForm -> SchForm
schLet binds body = RSList
  [ RSAtom "let"
  , RSList $ map (\(x,v) -> RSList [RSAtom x,v]) binds
  , body
  ]

schCon :: QName -> SchAtom
schCon x = T.singleton '\'' <> schName x

schConApp :: QName -> [SchForm] -> SchForm
schConApp c vs = RSList $
  [ RSAtom "list"
  , RSAtom (schCon c)
  ] ++ vs

schCase :: SchForm -> [SchForm] -> Maybe SchForm -> SchForm
schCase x cases maybeFallback = RSList $
  [ RSAtom "record-case"
  , x
  ] ++ cases ++
  [ RSList [ RSAtom "else" , fallback ] | fallback <- maybeToList maybeFallback
  ]

schUnit :: SchForm
schUnit = RSList [RSAtom "list"]

schDelay :: EvaluationStrategy -> SchForm -> SchForm
schDelay EagerEvaluation x = x
schDelay LazyEvaluation  x
  | RSList [RSAtom "force", y] <- x = y
  | otherwise                       = RSList [RSAtom "delay", x]

schForce :: EvaluationStrategy -> SchForm -> SchForm
schForce EagerEvaluation x = x
schForce LazyEvaluation  x
  | RSList [RSAtom "delay", y] <- x = y
  | otherwise                       = RSList [RSAtom "force", x]

schPreamble :: SchForm
schPreamble = RSList
  [ RSAtom "import"
  , RSList [ RSAtom "only" , RSList [RSAtom "chezscheme"] , RSAtom "record-case" ]
  ]

deriving instance Generic EvaluationStrategy
deriving instance NFData  EvaluationStrategy

data SchOptions = SchOptions
  { schEvaluation :: EvaluationStrategy
  }
  deriving (Generic, NFData)

data ToSchemeEnv = ToSchemeEnv
  { toSchemeOptions :: SchOptions
  , toSchemeVars    :: [SchAtom]
  }

initToSchemeEnv :: SchOptions -> ToSchemeEnv
initToSchemeEnv opts = ToSchemeEnv opts []

addVarBinding :: SchAtom -> ToSchemeEnv -> ToSchemeEnv
addVarBinding x env = env { toSchemeVars = x : toSchemeVars env }

data ToSchemeState = ToSchemeState
  { toSchemeFresh :: [Text]
  }

freshVars :: [SchAtom]
freshVars = concat [ map (<> i) xs | i <- "":(map (T.pack . show) [1..]) ]
  where
    xs = map T.singleton ['a'..'z']

initToSchemeState :: ToSchemeState
initToSchemeState = ToSchemeState freshVars

type ToSchemeM a = StateT ToSchemeState (ReaderT ToSchemeEnv TCM) a

freshSchAtom :: ToSchemeM SchAtom
freshSchAtom = do
  names <- gets toSchemeFresh
  case names of
    [] -> fail "No more variables!"
    (x:names') -> do
      modify $ \st -> st { toSchemeFresh = names' }
      return x

getEvaluationStrategy :: ToSchemeM EvaluationStrategy
getEvaluationStrategy = reader $ schEvaluation . toSchemeOptions

getVarName :: Int -> ToSchemeM SchAtom
getVarName i = reader $ (!! i) . toSchemeVars

withFreshVar :: (SchAtom -> ToSchemeM a) -> ToSchemeM a
withFreshVar f = do
  x <- freshSchAtom
  local (addVarBinding x) $ f x

withFreshVars :: Int -> ([SchAtom] -> ToSchemeM a) -> ToSchemeM a
withFreshVars i f
  | i <= 0    = f []
  | otherwise = withFreshVar $ \x -> withFreshVars (i-1) (f . (x:))

class ToScheme a b where
  toScheme :: a -> ToSchemeM b

instance ToScheme Definition (Maybe SchForm) where
  toScheme def
    | defNoCompilation def ||
      not (usableModality $ getModality def) = return Nothing
  toScheme def = do
    let f = defName def
    case theDef def of
      Axiom{} -> return $ Just $ schAxiom f
      GeneralizableVar{} -> return Nothing
      Function{} -> do
        strat <- getEvaluationStrategy
        maybeCompiled <- liftTCM $ toTreeless strat f
        case maybeCompiled of
          Just body -> Just <$> schDefine f <$> toScheme body
          Nothing   -> return Nothing
      Primitive{} -> return $ Just $ schAxiom f -- TODO!
      PrimitiveSort{} -> return Nothing
      Datatype{} -> return Nothing
      Record{} -> return Nothing
      Constructor{ conSrcCon = chead, conArity = nargs } -> do
        let c = conName chead
        withFreshVars nargs $ \xs ->
          return $ Just $ schDefine c $ schLambdas xs $ schConApp c $ map RSAtom xs

      AbstractDefn{} -> __IMPOSSIBLE__
      DataOrRecSig{} -> __IMPOSSIBLE__


instance ToScheme TTerm SchForm where
  toScheme v = case v of
    TVar i -> do
      name <- getVarName i
      strat <- getEvaluationStrategy
      return $ schForce strat $ RSAtom name
    TPrim p -> toScheme p
    TDef d -> return $ RSList [RSAtom (schName d)]
    TApp f args -> do
      f' <- toScheme f
      strat <- getEvaluationStrategy
      args' <- map (schDelay strat) <$> traverse toScheme args
      return $ schApps f' args'
    TLam v -> withFreshVar $ \x -> do
      body <- toScheme v
      return $ schLambda [x] body
    TLit l -> toScheme l
    TCon c -> return $ RSList [RSAtom (schName c)]
    TLet u v -> do
      expr <- toScheme u
      withFreshVar $ \x -> do
        body <- toScheme v
        return $ schLet [(x,expr)] body
    TCase i info v bs -> do
      strat <- getEvaluationStrategy
      x <- schForce strat . RSAtom <$> getVarName i
      cases <- traverse toScheme bs
      fallback <- if isUnreachable v
                  then return Nothing
                  else Just <$> toScheme v
      return $ schCase x cases fallback
    TUnit -> return schUnit
    TSort -> return schUnit
    TErased -> return schUnit
    TCoerce u -> toScheme u
    TError err -> toScheme err

    where
      isUnreachable v = v == TError TUnreachable

instance ToScheme TPrim SchForm where
  toScheme p = undefined

instance ToScheme Literal SchForm where
  toScheme lit = undefined

-- TODO: allow literal branches and guard branches
instance ToScheme TAlt SchForm where
  toScheme alt = case alt of
    TACon c nargs v -> withFreshVars nargs $ \xs -> do
      body <- toScheme v
      return $ RSList [RSList [RSAtom (schName c)], RSList (map RSAtom xs), body]

    TAGuard{} -> __IMPOSSIBLE__ -- TODO
    TALit{}   -> __IMPOSSIBLE__ -- TODO

instance ToScheme TError SchForm where
  toScheme err = case err of
    TUnreachable -> return $ schError "Panic!"
    TMeta s      -> return $ schError $ "encountered unsolved meta: " <> T.pack s
