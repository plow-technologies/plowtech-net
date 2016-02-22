#!/usr/bin/env runhaskell

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
        () <- cmd "cabal sandbox delete"
        return ()



    execute = wants >> rules


    wants = want $      [packageExecutableFile] <>
                        [sandboxDir]


    packageExecutableFile = hakyllProjectRootDir </>
                            hakyllExecDir </> hakyllSite
    sandboxDir = hakyllProjectRootDir </> sandbox







    -- Rules
    rules = packageExecutableFileRule <> sandboxDirRule


    sandboxDirRule = sandboxDir %> \_ -> cmdHakyll "cabal sandbox init"


    packageExecutableFileRule = packageExecutableFile %> \_ -> do
      () <- cmdHakyll "cabal update"
      () <- cmdHakyll "cabal install"
      () <- cmdHakyll "cabal configure"
      cmdHakyll "cabal build"


--------------------------------------------------
-- Helper functions
--------------------------------------------------

-- Cmd to run in the hakyll root dir
cmdHakyll = cmd Shell (Cwd hakyllProjectRootDir)
