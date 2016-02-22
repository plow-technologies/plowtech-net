#!/usr/bin/env runhaskell

import           Control.Monad              (unless)
import           Data.Monoid
import           Data.Monoid                ((<>))
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
--------------------------------------------------
-- Project Directories
--------------------------------------------------
-- The directory all the files will end up in
buildDir = "_build"


-- Where the hakyll project is located
hakyllProjectRootDir = "plowtech-hakyll"

-- normal cabal sandbox location
sandbox = ".cabal-sandbox"

-- location of executable to generate static site
hakyllExecDir = "dist" </> "build"</> "site"

-- root location of static files , images fonts etc
hakyllAssets = "assets"

siteDir = "_site"

stagingBucket =  "mockup.plowtech.net"



--------------------------------------------------
-- Project Key File Names
--------------------------------------------------


-- name of haskell executable
hakyllSite = "site"

-- name of produced website
index = "index" <.> "html"

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
        need [packageExecutableFile, sandboxDir,fullSiteDir,siteDir]
        putNormal "syncing up deply"
        cmd "rsync -r" (fullSiteDir) (".")
--        command_ ["aws s3 sync"] [siteDir, "s3:/" </> stagingBucket]

    -- Make Deploy
    deployarg = phony "deploy" $ do
        need [packageExecutableFile, sandboxDir,fullSiteDir,siteDir]
        putNormal "Preparing to deploy to staging"
        () <- cmd "rsync -r" (fullSiteDir) (".")
        command_ [] "aws s3 sync" [siteDir <> "/", "s3://" <> stagingBucket , "--region us-west-2"]


    -- View locally
    viewarg = phony "watch" $ do
      need [packageExecutableFile, sandboxDir,fullSiteDir]
      putNormal "starting Watch... go to http://localhost:8000"
      command_ [(Cwd hakyllProjectRootDir),FileStdout "hakyll-log.log"] (hakyllExecDir </> hakyllSite) ["watch"]



    -- Execute these things
    execute = wants >> rules


    wants = want $      [packageExecutableFile] <>
                        [sandboxDir] <>
                        [fullSiteDir]


    packageExecutableFile = hakyllProjectRootDir </>
                            hakyllExecDir </> hakyllSite
    sandboxDir = hakyllProjectRootDir </> sandbox
    fullSiteDir = hakyllProjectRootDir </> siteDir




    -- Rules
    rules = packageExecutableFileRule <>
            sandboxDirRule <>
            fullSiteDirRule <>
            siteDirRule <>


            -- Args
            cleanarg <>
            readyarg <>
            deployarg <>
            viewarg


    siteDirRule = siteDir %> \_ -> do
        need [fullSiteDir]
        val <- (doesDirectoryExist siteDir)
        unless val (cmd "mkdir" siteDir)



    sandboxDirRule = sandboxDir %> \_ -> cmdHakyll "cabal sandbox init"


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
