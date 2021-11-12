{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Code generation and test running in different languages. (Switchbox.)
module Data.Aeson.AutoType.CodeGen(
    Lang(..)
  , splitTypeByLabel
  , writeModule
  , runModule
  , defaultOutputFilename
  ) where

import           Data.Text(Text)
import qualified Data.HashMap.Strict as Map
import           Data.Aeson.AutoType.Type
import           System.Exit

import           Data.Aeson.AutoType.Split
import           Data.Aeson.AutoType.CodeGen.Haskell
import           Data.Aeson.AutoType.CodeGen.Elm
import           Data.Aeson.AutoType.CodeGen.Clang

-- | Available output languages.
data Lang = Haskell
          | HaskellStrict
          | Elm
          | Clang
          deriving(Eq)

-- | Splits initial type with a given label, into a mapping of object type names and object type structures.
splitTypeByLabel :: Lang -> Text -> Type -> Map Text Type
splitTypeByLabel Haskell       = splitTypeByLabelHaskell
splitTypeByLabel HaskellStrict = splitTypeByLabelHaskell
splitTypeByLabel Elm           = splitTypeByLabelElm
splitTypeByLabel Clang         = splitTypeByLabelClang

-- | Default output filname is used, when there is no explicit output file path, or it is "-" (stdout).
-- Default module name is consistent with it.
defaultOutputFilename :: Lang -> FilePath
defaultOutputFilename Haskell       = defaultHaskellFilename
defaultOutputFilename HaskellStrict = defaultHaskellFilename
defaultOutputFilename Elm           = defaultElmFilename
defaultOutputFilename Clang         = defaultClangFilename

-- | Write a Haskell module to an output file, or stdout if `-` filename is given.
writeModule :: Lang -> FilePath -> Text -> Map.HashMap Text Type -> IO ()
writeModule Haskell       = writeHaskellModule
writeModule HaskellStrict = writeHaskellModule
writeModule Elm           = writeElmModule
writeModule Clang         = writeClangModule

-- | Run module in a given language.
runModule :: Lang -> FilePath -> [String] -> IO ExitCode
runModule Haskell       = runHaskellModule
runModule HaskellStrict = runHaskellModuleStrict
runModule Elm           = runElmModule
runModule Clang         = runClangModule
