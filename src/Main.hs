module Main where

import Prelude hiding ( null , empty )

import Agda.Compiler.Backend
import Agda.Compiler.Common

import Agda.Main ( runAgda )

import Agda.Compiler.ToScheme

import Agda.TypeChecking.Pretty

import Agda.Utils.Either
import Agda.Utils.Functor
import Agda.Utils.Null
import Agda.Utils.Pretty ( prettyShow )

import Control.DeepSeq ( NFData )

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.SCargot
import Data.SCargot.Repr
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import GHC.Generics ( Generic )

main :: IO ()
main = runAgda [backend]

backend :: Backend
backend = Backend backend'

data SchOptions = SchOptions
  deriving (Generic, NFData)

backend' :: Backend' SchOptions SchOptions () () (Maybe SchForm)
backend' = Backend'
  { backendName           = "agda2scheme"
  , options               = SchOptions
  , commandLineFlags      = []
  , isEnabled             = \ _ -> True
  , preCompile            = schPreCompile
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ _ _ _ _ -> return $ Recompile ()
  , compileDef            = \ _ _ -> schCompileDef
  , postModule            = \ _ _ -> schPostModule
  , backendVersion        = Nothing
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  }

schPreCompile :: SchOptions -> TCM SchOptions
schPreCompile opts = return opts

schCompileDef :: IsMain -> Definition -> TCM (Maybe SchForm)
schCompileDef isMain def =
  toScheme def
  & (`evalStateT` initToSchemeState)
  & (`runReaderT` initToSchemeEnv)

schPostModule :: IsMain -> ModuleName -> [Maybe SchForm] -> TCM ()
schPostModule isMain modName defs = do
  let defToText = encodeOne (basicPrint id) . fromRich
      modText   = T.intercalate "\n\n" $ map defToText $ schPreamble : catMaybes defs
      fileName  = prettyShow (last $ mnameToList modName) ++ ".ss"
  liftIO $ T.writeFile fileName modText
