module Main ( main ) where

import Control.Applicative
import Data.Algorithm.Diff 
import Data.Maybe
import Data.Monoid
import Text.Printf
import Language.Haskell.Exts
import System.Environment

data VersionChange = NoChange 
                   | ChangeA 
                   | ChangeAB 
                   | ChangeABC
                     deriving (Ord,Eq,Show,Read)

data FileType = HSFile String
              | CabalFile String
                     deriving (Ord,Eq,Show,Read)

instance Monoid VersionChange where
    mempty = NoChange
    mappend = max
    mconcat = maximum


fromParseOk (ParseOk x) = x
diffModules (Module srcLoc1 modName1 optionPragmas1 warningText1 exportSpec1 importDecl1 decl1)
            (Module srcLoc2 modName2 optionPragmas2 warningText2 exportSpec2 importDecl2 decl2) = 	
                (getGroupedDiff (fromMaybe [] exportSpec1) 
                                (fromMaybe [] exportSpec2))



{-
	(()
--	,(getGroupedDiff srcLoc1 srcLoc2)
--	,(getGroupedDiff modName1 modName2)
	,(getGroupedDiff optionPragmas1 optionPragmas2)
--	,(getGroupedDiff warningText1 warningText2)
	,(getGroupedDiff (fromMaybe [] exportSpec1) (fromMaybe [] exportSpec2))
	,(getGroupedDiff importDecl1 importDecl2)
	,(getGroupedDiff decl1 decl2))
-}

pprint xs = mapM_ (\(st,vals) -> putStrLn (getInfo st) >> putStrLn (concatMap prettyPrint vals)) xs
	where
		getInfo F = ">>>"
		getInfo S = "<<<"
		getInfo B = "==="

{-
main = do
  (opts, files) <- parseOptions
  case files of
    (HSFile f1, HSFile f2) -> compareHSFiles f1 f2
    (CabalFile c1, CabalFile c2) -> compareCabalFiles c1 c2
    _ -> error $ printf "Unsupported filetype combination: %s" (show files)
-}

compareHSFiles f1 f2 = do
  x

compareCabalFiles f1 f2 = do
  f1' <- sort . getExportedModules . parseCabal <$> readFile f1
  f2' <- sort . getExportedModules . parseCabal <$> readFile f2
  
  

main = do
 	let getAndParse x = fromParseOk <$> parseFile x
	args <- getArgs
	f1 <- getAndParse (args !! 1)
	f2 <- getAndParse (args !! 0)
--	print (getGroupedDiff f1 f2)
	pprint (diffModules f1 f2)

