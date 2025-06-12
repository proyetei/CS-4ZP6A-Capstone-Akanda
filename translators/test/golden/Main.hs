-- | Golden tests.
module Main where

import Data.IntMap as IntMap

import System.FilePath

import Test.Tasty.Golden
import Test.Tasty

import Print.Agda qualified as Agda
import Print.Idris qualified as Idris
import Print.Lean qualified as Lean
import Print.Rocq qualified as Rocq

import Golden.Util.File

import Grammar
import Tests

-- * Testing utilities

-- | Convert a filename to a path to a snapshot file.
--
-- >>> snapshotPath "LetExample.agda"
-- "test/snapshots/LetExample.agda"
snapshotPath
  :: String
  -- ^ The base name to use for the snapshot file.
  -> FilePath
snapshotPath fname = "test" </> "snapshot" </> fname

-- | Convert a filename to a path to a staging file.
--
-- >>> stagingPath "LetExample.agda"
-- "test/staging/LetExample.agda"
stagingPath
  :: String
  -- ^ The name to use for the staging file.
  -> FilePath
stagingPath fname = "test" </> "staging" </> fname

-- | Create a golden test printing for a language.
printTestForLang
  :: String
  -- ^ The name of the language.
  -> (a -> String)
  -- ^ The printer to use for this thing.
  -> String
  -- ^ The extension to use for the saved snapshot and staging files.
  -> String
  -- ^ The base name to use for snapshot and staging files.
  -> a
  -- ^ The thing to print.
  -> TestTree
printTestForLang langName printer fileExt base syn =
  goldenVsFileDiff langName (\ref new -> ["diff" ,"-u", ref, new]) snapshotFile stagingFile do
    -- Make sure to encode our 'String' using UTF-8.
    createFile stagingFile
    writeBinaryFile stagingFile (printer syn)
  where
    stagingFile = stagingPath (base <.> fileExt)
    snapshotFile = snapshotPath (base <.> fileExt)

-- | Make a set of golden tests for a given module.
printModuleTestGroup
  :: TestName
  -- ^ The name of the test group.
  -> String
  -- ^ The base name to use for staging and snapshot files.
  -> Module
  -- ^ The abstract syntax of the module to print.
  -> TestTree
printModuleTestGroup groupName base syn =
  testGroup groupName
  [ printTestForLang "Agda" Agda.printModule ".agda" base syn
  , printTestForLang "Idris" Idris.printModule ".idr" base syn
  , printTestForLang "Lean" Lean.printModule ".lean" base syn
  , printTestForLang "Rocq" Rocq.printModule ".v" base syn
  ]

-- * Tests
--
-- These tests are all taken from the original @Tests.hs@ file,
-- and the size parameters were reverse engineered from @good_Output@.
-- All files in tests/snapshots originally also originated from @good_Output@,
-- so there is a throughline.

main :: IO ()
main = defaultMain $
  testGroup "Golden"
  [ printModuleTestGroup "Sequential let bindings" "LetExample" (tests IntMap.! 1 $ 5)

  , printModuleTestGroup "Repeated addition and let Bindings" "LetAddExample" (tests IntMap.! 2 $ 5)
  , printModuleTestGroup "Nested function declarations" "NestedFunction" (tests IntMap.! 3 $ 5)
  , printModuleTestGroup "Simple data declarations" "DataSimpleDeclarations" (tests IntMap.! 4 $ 5)
  , printModuleTestGroup "Single long identifier" "LongIdentifier" (tests IntMap.! 5 $ 5)

  , printModuleTestGroup "Record with lots of dependent fields" "Fields_DependentRecordModule" (tests IntMap.! 6 $ 5)
  , printModuleTestGroup "Sequence of dependent record declarations" "ChainDef_DependentRecordModule" (tests IntMap.! 7 $ 5)
  , printModuleTestGroup "Record with lots of parameters" "Parameters_DependentRecordModule" (tests IntMap.! 8 $ 5)

  , printModuleTestGroup "File with lots of newlines" "NewlineFile" (tests IntMap.! 9 $ 5)
  , printModuleTestGroup "Record declaration with non-dependent fields" "Fields_NonDependentRecordModule" (tests IntMap.! 10 $ 5)
  , printModuleTestGroup "Sequence of non-dependent record declarations" "ChainDefFields_NonDependentRecordModule" (tests IntMap.! 11 $ 5)

  , printModuleTestGroup "Non indexed, non parameterised datatype with many constructors" "Constructors_Datatypes" (tests IntMap.! 12 $ 5)
  , printModuleTestGroup "Non indexed, parameterised datatype with many constructors" "Parameters_Datatypes" (tests IntMap.! 13 $ 5)

  , printModuleTestGroup "Lots of definitions" "FirstLast_VariableModule" (tests IntMap.! 14 $ 5)
  , printModuleTestGroup "Lots of dependent definitions" "DeepDependency_VariableModule" (tests IntMap.! 15 $ 5)

  , printModuleTestGroup "Datatype with lots of indicies" "DataImplicitIndices" (tests IntMap.! 16 $ 5)
  , printModuleTestGroup "A file with a single long line" "SingleLongLine" (tests IntMap.! 17 $ 5)
  , printModuleTestGroup "A datatype with lots of 'Type'-valued parameters" "ConstructorsParameters_Datatypes" (tests IntMap.! 18 $ 5)
  , printModuleTestGroup "A datatype with a lot of indices that are used in constructors" "IndicesConstructors_Datatypes" (tests IntMap.! 19 $ 5)
  , printModuleTestGroup "A datatype with lots of parameters and indices" "IndicesParameters_Datatypes" (tests IntMap.! 20 $ 5)
  , printModuleTestGroup "A pattern matching function with lots of cases" "Pattern_Matching_Datatypes" (tests IntMap.! 21 $ 5)
  ]
