module Main where

import Language.Haskell.Exts
import Data.Algorithm.Diff 
import Control.Applicative
import Data.Maybe
import System.Environment

fromParseOk (ParseOk x) = x
diffModules (Module srcLoc1 modName1 optionPragmas1 warningText1 exportSpec1 importDecl1 decl1)
            (Module srcLoc2 modName2 optionPragmas2 warningText2 exportSpec2 importDecl2 decl2) = 	
	(getGroupedDiff decl1 decl2)

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

main = do
	let getAndParse x = fromParseOk <$> parseFile x
	args <- getArgs
	f1 <- getAndParse (args !! 0)
	f2 <- getAndParse (args !! 1)
--	print (getGroupedDiff f1 f2)
	pprint (diffModules f1 f2)

