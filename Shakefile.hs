#!/usr/bin/env runhaskell

import           Control.Monad              (unless)
import qualified Data.List                  as L
import           Data.Monoid
import           Data.Monoid                ((<>))
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import qualified System.IO                  as IO
--------------------------------------------------
-- Project Directories
--------------------------------------------------
-- The directory all the files will end up in
buildDir = "_build"


-- Where the hakyll project is located
hakyllProjectRootDir = "plowtech-hakyll"

-- normal cabal sandbox location
sandbox = ".cabal-sandbox" </> "add-source-timestamps"

-- location of executable to generate static site
hakyllExecDir = "dist" </> "build"</> "site"

-- root location of static files , images fonts etc
hakyllAssets = "assets"

siteDir = "_site"

siteFile = "_site" </> "index.html"

productDir = hakyllProjectRootDir </> "products"

stagingBucket =  "mockup.plowtech.net"
productionBucket = "www.plowtech.net"




--------------------------------------------------
-- Project Key File Names
--------------------------------------------------


-- name of haskell executable
hakyllSite = "site"

-- name of produced website
index = "index" <.> "html"




--------------------------------------------------
-- Approved Product selection
--------------------------------------------------
-- | Place products that have been approved for production here.
-- They are the only ones that will actually release to production.

productionReadyFileName = "production-ready-products.txt"

removeNonApprovedProducts  = do
  contents <- getDirectoryContents productDir
  productFile <- readFileLines productionReadyFileName
  let filesToDelete = L.filter filterFunction contents :: [FilePath]
      deleteAllNonApprovedFiles :: FilePath -> Action ()
      deleteAllNonApprovedFiles file = (command_ [(Cwd productDir)] "rm" [file])
      filterFunction f = L.notElem f productFile
  traverse deleteAllNonApprovedFiles filesToDelete




--------------------------------------------------
-- Application
--------------------------------------------------
main :: IO ()
main = (shakeArgs shakeOptions {shakeFiles=buildDir}) execute
  where






    -- Cleanup --------------------------------------------------
    cleanarg = phony "clean" $ do
        putNormal "cleaning files in build"
        putNormal "removing submodules ..."
        command_ [(Cwd hakyllProjectRootDir)] (hakyllExecDir </> hakyllSite) ["clean"]
        () <- cmdHakyll "cabal clean"
        () <- cmdHakyll "cabal sandbox delete"
        return ()



    -- Deploy --------------------------------------------------



    -- Make Ready For Deployment
    readyarg = phony "ready" $ do
        need [packageExecutableFile, sandboxDir,fullSiteDir,siteFile]
        putNormal "syncing up deply"
        cmd "rsync -r" (fullSiteDir) (".")
--        command_ ["aws s3 sync"] [siteDir, "s3:/" </> stagingBucket]
    readyargProduction = phony "readyprod" $ do
      removeNonApprovedProducts
      need [packageExecutableFile, sandboxDir,fullSiteDir,siteDir]
      putNormal "syncing up deply"
      cmd "rsync -r" (fullSiteDir) (".")


    -- Make Deploy
    deployStagingarg = phony "deploy-staging" $ do
        need [packageExecutableFile, sandboxDir,fullSiteFile,siteFile]
        putNormal "Preparing to deploy to staging"
        () <- cmd "rsync -r" (fullSiteDir) (".")
        command_ [Shell] "aws" ["s3","sync", siteDir <> "/", "s3://" <> stagingBucket , "--region us-west-2"]

    -- Make Deploy
    deployProductionarg = phony "deploy-production" $ do
        removeNonApprovedProducts
        need [packageExecutableFile, sandboxDir,fullSiteFile,siteFile]
        putNormal "Preparing to deploy to production"
        () <- cmd "rsync -r" (fullSiteDir) (".")
        command_ [Shell] "aws" ["s3","sync", siteDir <> "/", "s3://" <> productionBucket , "--region us-west-2"]


    -- View locally
    viewarg = phony "watch" $ do
      need [packageExecutableFile, sandboxDir,fullSiteDir]
      putNormal "starting Watch... go to http://localhost:8000"
      command_ [(Cwd hakyllProjectRootDir),FileStdout "hakyll-log.log"] (hakyllExecDir </> hakyllSite) ["clean"]
      command_ [(Cwd hakyllProjectRootDir),FileStdout "hakyll-log.log"] (hakyllExecDir </> hakyllSite) ["build"]
      command_ [(Cwd hakyllProjectRootDir),FileStdout "hakyll-log.log"] (hakyllExecDir </> hakyllSite) ["watch"]
   
    -- Site only clean
    siteCleanarg = phony "site-clean" $ do
      need [packageExecutableFile, sandboxDir,fullSiteDir]
      putNormal "starting clean"
      command_ [(Cwd hakyllProjectRootDir),FileStdout "hakyll-log.log"] (hakyllExecDir </> hakyllSite) ["clean"]



    -- Execute these things
    execute = wants >> rules


    wants = want $      [packageExecutableFile] <>
                        [sandboxDir] <> 
                        [fullSiteFile]


    packageExecutableFile = hakyllProjectRootDir </>
                            hakyllExecDir </> hakyllSite
    sandboxDir = hakyllProjectRootDir </> sandbox
    fullSiteDir = hakyllProjectRootDir </> siteDir
    fullSiteFile = hakyllProjectRootDir </> siteDir </> "index.html"



    -- Rules
    rules = packageExecutableFileRule <>
            sandboxDirRule <>
            fullSiteDirRule <>
            siteDirRule <>


            -- Args
            cleanarg <>
            readyarg <>
            readyargProduction <>
            deployStagingarg <>
            deployProductionarg <>
            viewarg <> 
            siteCleanarg


    siteDirRule = siteDir %> \_ -> do
        need [fullSiteDir]
        val <- (doesDirectoryExist siteDir)
        unless val (cmd "mkdir" siteDir)



    sandboxDirRule = sandboxDir %> \_ -> do
                       cmdHakyll "cabal sandbox init"


    packageExecutableFileRule = packageExecutableFile %> \_ -> do
      need [sandboxDir]
      () <- cmdHakyll "cabal update"
      () <- cmdHakyll "cabal install"
      () <- cmdHakyll "cabal configure"
      cmdHakyll "cabal build"





    fullSiteDirRule = fullSiteDir %> \_ -> do
      need [sandboxDir, packageExecutableFile]
      command_ [(Cwd hakyllProjectRootDir)] (hakyllExecDir </> hakyllSite) ["clean"]
      command_ [(Cwd hakyllProjectRootDir)] (hakyllExecDir </> hakyllSite) ["build"]
 --------------------------------------------------
-- Helper functions
--------------------------------------------------

-- Cmd to run in the hakyll root dir
cmdHakyll = cmd (Cwd hakyllProjectRootDir) Shell

--                                                            () <- cmdHakyll "aws s3 sync"   [siteDir] s3://$BUCKET/ --region us-west-2

runOnlyWhenFolderNotPresent fp cmd = do
   rslt <- (doesDirectoryExist fp)
   if rslt
      then cmd
      else return ()
