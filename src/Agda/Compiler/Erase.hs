{-# LANGUAGE PatternSynonyms #-}

-- This is a copy of the module Agda.Compiler.Treeless.Erase from the
-- main Agda repository, but without an export list so all functions
-- can be used.

module Agda.Compiler.Erase where

import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Agda.Syntax.Common
import Agda.Syntax.Internal as I
import Agda.Syntax.Position
import Agda.Syntax.Treeless
import Agda.Syntax.Literal

import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Monad as I
import Agda.TypeChecking.Telescope
import Agda.TypeChecking.Datatypes
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Primitive

import Agda.Compiler.Backend
import Agda.Compiler.Treeless.Subst
import Agda.Compiler.Treeless.Unused

import Agda.Utils.Functor
import Agda.Utils.Lens
import Agda.Utils.Maybe
import Agda.Utils.Memo
import Agda.Utils.Monad
import Agda.Utils.Pretty (prettyShow)
import Agda.Utils.IntSet.Infinite (IntSet)
import qualified Agda.Utils.IntSet.Infinite as IntSet

import Agda.Utils.Impossible

-- | State of the eraser.
data ESt = ESt
  { _funMap  :: Map QName FunInfo
      -- ^ Memoize computed `FunInfo` for functions/constructors/... `QName`.
  , _typeMap :: Map QName TypeInfo
      -- ^ Memoize computed `TypeInfo` for data/record types `QName`.
  }

funMap :: Lens' (Map QName FunInfo) ESt
funMap f r = f (_funMap r) <&> \ a -> r { _funMap = a }

typeMap :: Lens' (Map QName TypeInfo) ESt
typeMap f r = f (_typeMap r) <&> \ a -> r { _typeMap = a }

-- | Eraser monad.
type E = StateT ESt TCM

runE :: E a -> TCM a
runE m = evalStateT m (ESt Map.empty Map.empty)

data TypeInfo = Empty | Erasable | NotErasable
  deriving (Eq, Show)

sumTypeInfo :: [TypeInfo] -> TypeInfo
sumTypeInfo is = foldr plus Empty is
  where
    plus Empty       r           = r
    plus r           Empty       = r
    plus Erasable    r           = r
    plus r           Erasable    = r
    plus NotErasable NotErasable = NotErasable

erasable :: TypeInfo -> Bool
erasable Erasable    = True
erasable Empty       = True
erasable NotErasable = False

type FunInfo = ([TypeInfo], TypeInfo)

getFunInfo :: QName -> E FunInfo
getFunInfo q = memo (funMap . key q) $ getInfo q
  where
    getInfo :: QName -> E FunInfo
    getInfo q = do
      (rs, t) <- do
        (tel, t) <- lift $ typeWithoutParams q
        is     <- mapM (getTypeInfo . snd . dget) tel
        used   <- lift $ (++ repeat ArgUsed) . fromMaybe [] <$> getCompiledArgUse q
        forced <- lift $ (++ repeat NotForced) <$> getForcedArgs q
        return (zipWith3 (uncurry . mkR . getModality) tel (zip forced used) is, t)
      h <- if isAbsurdLambdaName q then pure Erasable else getTypeInfo t
      lift $ reportSLn "treeless.opt.erase.info" 50 $ "type info for " ++ prettyShow q ++ ": " ++ show rs ++ " -> " ++ show h
      lift $ setErasedConArgs q $ map erasable rs
      return (rs, h)

    -- Treat empty, erasable, or unused arguments as Erasable
    mkR :: Modality -> IsForced -> ArgUsage -> TypeInfo -> TypeInfo
    mkR m f u i
      | not (usableModality m) = Erasable
      | ArgUnused <- u         = Erasable
      | Forced <- f            = Erasable
      | otherwise              = i

telListView :: Type -> TCM (ListTel, Type)
telListView t = do
  TelV tel t <- telViewPath t
  return (telToList tel, t)

typeWithoutParams :: QName -> TCM (ListTel, Type)
typeWithoutParams q = do
  def <- getConstInfo q
  let d = case I.theDef def of
        Function{ funProjection = Just Projection{ projIndex = i } } -> i - 1
        Constructor{ conPars = n } -> n
        _                          -> 0
  first (drop d) <$> telListView (defType def)

getTypeInfo :: Type -> E TypeInfo
getTypeInfo t0 = do
  (tel, t) <- lift $ telListView t0
  et <- case I.unEl t of
    I.Def d _ -> do
      -- #2916: Only update the memo table for d. Results for other types are
      -- under the assumption that d is erasable!
      oldMap <- use typeMap
      dInfo <- typeInfo d
      typeMap .= Map.insert d dInfo oldMap
      return dInfo
    Sort{}    -> return Erasable
    _         -> return NotErasable
  is <- mapM (getTypeInfo . snd . dget) tel
  let e | Empty `elem` is = Erasable
        | null is         = et        -- TODO: guard should really be "all inhabited is"
        | et == Empty     = Erasable
        | otherwise       = et
  lift $ reportSDoc "treeless.opt.erase.type" 50 $ prettyTCM t0 <+> text ("is " ++ show e)
  return e
  where
  typeInfo :: QName -> E TypeInfo
  typeInfo q = ifM (erasureForbidden q) (return NotErasable) $ {-else-} do
    memoRec (typeMap . key q) Erasable $ do  -- assume recursive occurrences are erasable
      mId    <- lift $ getName' builtinId
      msizes <- lift $ mapM getBuiltinName
                         [builtinSize, builtinSizeLt]
      def    <- lift $ getConstInfo q
      let mcs = case I.theDef def of
                  I.Datatype{ dataCons = cs } -> Just cs
                  I.Record{ recConHead = c }  -> Just [conName c]
                  _                           -> Nothing
      case mcs of
        _ | Just q == mId        -> return NotErasable
        _ | Just q `elem` msizes -> return Erasable
        Just [c] -> do
          (ts, _) <- lift $ typeWithoutParams c
          let rs = map getModality ts
          is <- mapM (getTypeInfo . snd . dget) ts
          let er = and [ erasable i || not (usableModality r) | (i, r) <- zip is rs ]
          return $ if er then Erasable else NotErasable
        Just []      -> return Empty
        Just (_:_:_) -> return NotErasable
        Nothing ->
          case I.theDef def of
            I.Function{ funClauses = cs } ->
              sumTypeInfo <$> mapM (maybe (return Empty) (getTypeInfo . El __DUMMY_SORT__) . clauseBody) cs
            _ -> return NotErasable
  -- The backend also has a say whether a type is eraseable or not.
  erasureForbidden :: QName -> E Bool
  erasureForbidden q = lift $ not <$> activeBackendMayEraseType q
