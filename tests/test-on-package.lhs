#!/usr/bin/runhaskell

This is a tool script for checking various Hackage packages using package-vt.
To use it you will need working set of:
- tar
- cabal-install
- package-vt
All tools need to be in PATH. If not, you can change command manually in the code below.

Prior to running this script you should consider running "get-package-index.sh".
It will fetch 00-index.tar.gz from Hackage and unpack it into relevant directory.

> module Main where

> import System.FilePath
> import System.Directory
> import System.Environment
> import System.Process
> import System.Exit

> import Control.Applicative
> import Control.Monad
> import Data.Char ( toLower )
> import Data.List ( sort, sortBy )
> import Data.Ord ( comparing )
> import Data.Version
> import Text.ParserCombinators.ReadP ( readP_to_S )

> import Text.Printf

> packageCache = "packages" -- directory for all packages

> dirsFromDirectoryContents dir = filter (\fn -> not (fn `elem` [".",".."])) <$> 
>                                 (filterM doesDirectoryExist =<< getDirectoryContents dir)

> main = do
>   packages <- getArgs
>   when (null packages) (putStrLn "Supply one or more package name to test on." >> exitFailure)
>   mapM_ runPackage packages
> {-
>   cont <- dirsFromDirectoryContents "."
>   mapM_ runDir cont
>  -}

> exitWhenFail :: (IO ExitCode -> IO ())
> exitWhenFail ecM = do
>   ec <- ecM
>   case ec of
>     ExitSuccess -> return ()
>     ef@(ExitFailure x) -> exitWith ef

> unlessM :: (Monad m) => (m Bool) -> (m ()) -> (m ())
> unlessM bM action = do
>   b <- bM
>   unless b action

> (<->) :: String -> String -> String
> pre <-> suf = pre ++ "-" ++ suf

> withCurrentDirectory dir act = do
>   old <- getCurrentDirectory
>   setCurrentDirectory dir
>   act
>   setCurrentDirectory old

> runPackage package = withCurrentDirectory (packageCache </> package) $ do
>   let isVersionLike "." = False
>       isVersionLike ".." = False
>       isVersionLike fp = not (any (\x -> not (x `elem` ['0'..'9'])) (filter (/='.') fp))
>                          
>       checkDownloadVersion :: FilePath -> IO ()
>       checkDownloadVersion ver = let packVer = package <-> ver in
>                                  unlessM (doesDirectoryExist (ver </> packVer))
>                                          (exitWhenFail (system $ printf "cabal unpack %s -d %s" packVer ver))
>       runPackageVT older newer = do
>                        let aux ver = ver </> (package <-> ver) </> (package <.> "cabal")
>                        putStrLn $ printf ">>> Testing package-vt on %s and %s" (package <-> older) (package <-> newer)
>                        rawSystem "package-vt" [aux older, aux newer] >>= print
>                        putStrLn ""
>       sortVersions vs = map snd . sort . map (\v -> ((readP_to_S parseVersion v),v)) $ vs
>   
>   versions <- sortVersions . filter isVersionLike <$> (filterM doesDirectoryExist =<< getDirectoryContents ".")
>   putStrLn $ printf "%s => %s" package (show $ (versions :: [FilePath]))
>   if length versions < 2 
>      then putStrLn (printf ">>> Only one version, nothing to compare with ==> skipping package %s" package)
>      else do
>            putStrLn ">>> Download check." >> mapM_ checkDownloadVersion versions
>            zipWithM_ (\older newer -> runPackageVT older newer) versions (tail versions)
>                     

> getCabal :: FilePath -> IO FilePath
> getCabal dir = head . map (dir </>) . filter (\fn -> ".cabal" == takeExtension fn) <$> getDirectoryContents dir

> runDir dir = do
>              print dir
>              cont <- sortBy (comparing (map toLower)) <$> dirsFromDirectoryContents dir
>              zipWithM_ (\d1 d2 -> do
>                          print (d1,d2) 
>                          c1 <- getCabal d1
>                          c2 <- getCabal d2
>                          system (printf "package-vt \"%s\" \"%s\"" c1 c2)
>                          return ()
>                        ) cont (tail cont)
