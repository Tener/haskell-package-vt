module Main ( main ) where

import Control.Applicative ( (<$>) )
import Data.Algorithm.Diff ( getGroupedDiff, DI(..) )
import Data.Maybe ( catMaybes, fromJust, fromMaybe )
import Data.Monoid
import Data.List ( sort )
import Text.Printf ( printf )
import Language.Haskell.Exts -- too many things to list
import System.Environment ( getArgs )
import System.FilePath

-- Cabal stuff

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.ModuleName as ModuleName


data VersionChange = NoChange
                   | ChangeABC { info, details :: String }
                   | ChangeAB { info, details :: String }
                     deriving (Eq,Show,Read,Ord)

data FileType = HSFile String
              | CabalFile String
                     deriving (Ord,Eq,Show,Read)

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
    [HSFile f1, HSFile f2] -> catMaybes . (:[]) <$> compareHSFiles f1 f2
    [CabalFile c1, CabalFile c2] -> compareCabalFiles c1 c2
    _ -> error $ printf "Unsupported filetype combination: %s" (show files)
  
  putStrLn . concatMap pprintVC $ r
  putStrLn . pprintVC . mconcat $ r

-- | pretty print 'VersionChange'

pprintVC :: VersionChange -> String
pprintVC NoChange = "NoChange : no changes suggested."
pprintVC (ChangeABC info details) = printf "ChangeABC : %s\n\t%s" info details
pprintVC (ChangeAB info details) = printf "ChangeAB : %s\n\t%s" info details

--  Predicates for matching with certain diff cases
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

-- | Compare two package descriptions from 
compareCabalFiles :: FilePath -> FilePath -> IO [VersionChange]
compareCabalFiles fOld fNew = do
  let readParse :: FilePath -> IO (FilePath,[FilePath])
      readParse fn = do
        cabal <- readPackageDescription Verbosity.verbose fn
        let getExportedModules = exposedModules . condTreeData . fromJust . condLibrary 
            -- TODO: actually use multiple dirs, not just one.
            getSourceDir = head . hsSourceDirs . libBuildInfo . condTreeData . fromJust . condLibrary
            
            modules = map ModuleName.toFilePath . sort . getExportedModules $ cabal
            sourceDir = getSourceDir $ cabal
        return (sourceDir,modules)

  (dirO, modsOld) <- readParse fOld
  (dirN, modsNew) <- readParse fNew
  let (vc,diff) = diffGetVC "Module(s) removed/renamed." 
                              ("Module(s) added." ++ 
                               "(Consider AB change if new modules are likely to cause name collisions)")
                               modsOld
                               modsNew
                               

  hsDiffs <- sequence [ compareHSFiles (dropFileName fOld </> dirO </> f <.> "hs") 
                                       (dropFileName fNew </> dirN </> f <.> "hs") | 
                                       f <- concat . map snd . filter isBoth $ diff]
  return (sort . catMaybes $ vc : hsDiffs)

-- | Compare to .hs files. Return any changes found. 
--   TODO: consider using language extensions declared in .cabal file.
compareHSFiles :: FilePath -> FilePath -> IO (Maybe VersionChange)
compareHSFiles fOldPath fNewPath = do
  let readAndParse fn = fromParseResult <$> parseFile fn
  fOld@(Module srcLoc1 modName1 optionPragmas1 warningText1 exportSpec1 importDecl1 decl1) <- readAndParse fOldPath
  fNew@(Module srcLoc2 modName2 optionPragmas2 warningText2 exportSpec2 importDecl2 decl2) <- readAndParse fNewPath

  let info1 = "One or more entity removed/renamed from module " ++ show modName1
      info2 = "Entity added to module " ++ show modName1
      (diff,_) = diffGetVC info1 info2
                           (fromMaybe (error "exportSpec1 empty") exportSpec1)
                           (fromMaybe (error "exportSpec2 empty") exportSpec2)

  return diff
