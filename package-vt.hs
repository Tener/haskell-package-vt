{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-} 
module Main ( main ) where

import Control.Applicative ( (<$>) )
import Control.Exception
import Data.Algorithm.Diff ( getGroupedDiff, DI(..) )
import Data.Maybe ( catMaybes, fromJust, fromMaybe )
import Data.Monoid
import Data.Data
import Data.Typeable
import Data.List ( sort, group )
import Text.Printf ( printf )
import Language.Haskell.Exts -- too many things to list
import System.Environment ( getArgs )
import System.FilePath

-- Cabal stuff

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.PackageDescription.Configuration
import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.ModuleName as ModuleName


data VersionChange = NoChange
                   | ChangeABC { info, details :: String }
                   | ChangeAB { info, details :: String }
                     deriving (Eq,Show,Read,Ord)

data FileType = HSFile String
              | CabalFile String
                     deriving (Ord,Eq,Show,Read)

-- | Custom datatype for parse exceptions
data ParseError = ParseError String deriving (Ord,Eq,Data,Show,Read,Typeable)
instance Exception ParseError

-- | Monoid instance for VersionChange describes which change has stronger priority.
instance Monoid VersionChange where
    mempty = NoChange

    mappend x y = if val x <= val y
                   then y 
                   else x 
                       where
                         val NoChange{} = 0
                         val ChangeABC{} = 1
                         val ChangeAB{} = 2

main :: IO ()
main = do
  (f1:f2:_) <- getArgs
  let files = map chooseFileType [f1,f2]
      chooseFileType fn = case takeExtension fn of
                            ".hs" -> HSFile fn
                            ".lhs" -> HSFile fn
                            ".cabal" -> CabalFile fn
                            ext -> error $ printf "Bad file extension: %s" (show ext)

  r <- case files of
    [HSFile f1, HSFile f2] -> catMaybes . (:[]) <$> compareHSFiles [] f1 f2
    [CabalFile c1, CabalFile c2] -> compareCabalFiles c1 c2
    _ -> error $ printf "Unsupported filetype combination: %s" (show files)
  
  putStrLn "Single changes:"
  putStrLn . concatMap pprintVC $ sort r
  putStrLn "Biggest change comes from:"
  putStrLn . pprintVC . mconcat $ r

-- | pretty print 'VersionChange'

pprintVC :: VersionChange -> String
pprintVC NoChange = "NoChange : no changes suggested."
pprintVC (ChangeABC info details) = printf "ChangeABC : %s\n---\n%s\n---" info details
pprintVC (ChangeAB info details) = printf "ChangeAB : %s\n---\n%s\n---" info details

-- |  Predicates for matching with certain diff cases
isFirst, isSecond, isBoth :: (DI,a) -> Bool
isFirst  = (==F) . fst
isSecond = (==S) . fst
isBoth   = (==B) . fst

-- | Utility function used in compareCabalFiles and compareHSFiles
diffGetVC :: (Eq a, Show a) => String -> String -> [a] -> [a] -> (Maybe VersionChange, [(DI,[a])])
diffGetVC info1 info2 xs ys = 
    let diff = getGroupedDiff xs ys
        aux pred = unlines . map (unlines . map show . snd) . filter pred $ diff
        r | any isFirst diff = Just $ ChangeAB info1 (aux isFirst)
          | any isSecond diff = Just $ ChangeABC info2 (aux isSecond)
          | otherwise = Nothing
    in
      (r,diff)

-- | nubOrd
nubOrd :: (Ord a) => [a] -> [a]
nubOrd = map head . group . sort 

-- | Compare two package descriptions from 
compareCabalFiles :: FilePath -> FilePath -> IO [VersionChange]
compareCabalFiles fOld fNew = do
  let readParse :: FilePath -> IO (FilePath,[FilePath])
      readParse fn = do
        cabal <- flattenPackageDescription <$> readPackageDescription Verbosity.normal fn
        let --
            modules = map ModuleName.toFilePath . sort . libModules $ cabal
            sourceDirs = (nubOrd . sort . concatMap hsSourceDirs . allBuildInfo $ cabal) ++ ["."]
            -- XXX: use multiple source dirs
        return (head sourceDirs,modules)

  (dirO, modsOld) <- readParse fOld
  (dirN, modsNew) <- readParse fNew
  let (vc,diff) = diffGetVC "Module(s) removed/renamed." 
                              ("Module(s) added." ++ 
                               "(Consider AB change if new modules are likely to cause name collisions)")
                               modsOld
                               modsNew
                               

  hsDiffs <- sequence [ compareHSFiles (glasgowExts ++ [CPP,MultiParamTypeClasses])
                                       (dropFileName fOld </> dirO </> f <.> "hs") 
                                       (dropFileName fNew </> dirN </> f <.> "hs") | 
                                       f <- concatMap snd . filter isBoth $ diff]
  return (sort . catMaybes $ vc : hsDiffs)

-- | Compare two .[l]hs files. Return any changes found. 
compareHSFiles :: [Extension] -> FilePath -> FilePath -> IO (Maybe VersionChange)
compareHSFiles exts fOldPath fNewPath = do
  let onIOError :: IOException -> IO (Maybe VersionChange)
      onIOError e = do
        putStrLn (printf "IO Exception: %s" (show (e :: IOException)))
        putStrLn (printf "Skipping modules %s, %s" (show fOldPath) (show fNewPath))
        return Nothing

      onParseError :: ParseError -> IO (Maybe VersionChange)
      onParseError (ParseError str) = do
        putStrLn str
        return Nothing
  handle onParseError $
   handle onIOError $  
--  handle (\NonTermination -> error "Non termination") $
    do
     let fromParseResult :: ((ParseResult a) -> a) -> (ParseResult a) -> a
         fromParseResult onErr (ParseOk a) = a
         fromParseResult onErr err@(ParseFailed _ _) = onErr err
         readAndParse fn = fromParseResult (\(ParseFailed src err) -> throw (ParseError (err ++ " : " ++ show src)))  <$> parseFileWithExts exts fn
     fOld@(Module srcLoc1 modName1 optionPragmas1 warningText1 exportSpec1 importDecl1 decl1) <- readAndParse fOldPath
     fNew@(Module srcLoc2 modName2 optionPragmas2 warningText2 exportSpec2 importDecl2 decl2) <- readAndParse fNewPath
   
     let info1 = "One or more entity removed/renamed from module " ++ show modName1
         info2 = "Entity added to module " ++ show modName1
         diff = if (exportSpec1 == Nothing) && (exportSpec2 == Nothing) then Nothing else 
                       fst $ diffGetVC info1 info2
                              (fromMaybe (error "exportSpec1 empty") exportSpec1)
                              (fromMaybe (error "exportSpec2 empty") exportSpec2)
   
     return diff
