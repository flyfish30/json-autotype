{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Wrappers for generating prologue and epilogue code in Haskell.
module Data.Aeson.AutoType.CodeGen.Haskell(
    writeHaskellModule
  , runHaskellModule
  , runHaskellModuleStrict
  , defaultHaskellFilename
  , splitTypeByLabelHaskell
  ) where

import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import           Data.Text hiding (unwords)
import qualified Data.HashMap.Strict as Map
import           Control.Arrow               (first)
import           Control.Exception (assert)
import           Data.Default
import           Data.Monoid                 ((<>))
import           System.FilePath
import           System.IO
import           System.Process                 (system)
import qualified System.Environment             (lookupEnv)
import           System.Exit                    (ExitCode)

import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.CodeGen.Generic(src)
import           Data.Aeson.AutoType.CodeGen.HaskellFormat
import           Data.Aeson.AutoType.Util

import qualified Language.Haskell.RunHaskellModule as Run

-- | Default output filname is used, when there is no explicit output file path, or it is "-" (stdout).
-- Default module name is consistent with it.
defaultHaskellFilename :: FilePath
defaultHaskellFilename = "JSONTypes.hs"

header :: Text -> Text
header moduleName = [src|
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
-- | DO NOT EDIT THIS FILE MANUALLY!
--   It was automatically generated by `json-autotype`.

module |] <> capitalize moduleName <> [src| where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(eitherDecode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid((<>))
import           Data.Text (Text)
import qualified GHC.Generics
|]

epilogue :: Text -> Text
epilogue toplevelName = [src|
parse :: FilePath -> IO |] <> toplevelName <> [src|
parse filename = do
    input <- BSL.readFile filename
    case eitherDecode input of
      Left errTop -> fatal $ case (eitherDecode input :: Either String Value) of
                           Left  err -> "Invalid JSON file: " ++ filename ++ "\n   " ++ err
                           Right _   -> "Mismatched JSON value from file: " ++ filename
                                     ++ "\n" ++ errTop
      Right r     -> return (r :: |] <> toplevelName <> ")" <> [src|
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess
|]

-- | Write a Haskell module to an output file, or stdout if `-` filename is given.
writeHaskellModule :: FilePath -> Text -> Map.HashMap Text Type -> IO ()
writeHaskellModule outputFilename toplevelName types =
    withFileOrHandle outputFilename WriteMode stdout $ \hOut ->
      assert (extension == ".hs") $ do
        Text.hPutStrLn hOut $ header $ Text.pack moduleName
        -- We write types as Haskell type declarations to output handle
        Text.hPutStrLn hOut $ displaySplitTypes types
        Text.hPutStrLn hOut $ epilogue toplevelName
  where
    (moduleName, extension) =
       first normalizeTypeName'     $
       splitExtension               $
       if     outputFilename == "-"
         then defaultHaskellFilename
         else outputFilename
    normalizeTypeName' = Text.unpack . normalizeTypeName . Text.pack

runHaskellModule :: FilePath -> [String] -> IO ExitCode
runHaskellModule = Run.runHaskellModule

runHaskellModuleStrict :: FilePath -> [String] -> IO ExitCode
runHaskellModuleStrict = Run.runHaskellModule' opts
  where
      opts = def { Run.compileArgs = ["-Wall", "-Werror"] }

