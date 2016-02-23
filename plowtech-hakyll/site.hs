--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens                           hiding (Context)
import qualified Data.Attoparsec.Text                   as AttoParsec
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            (mappend)
import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import qualified Data.Text.IO                           as Text
import           Debug.Trace
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "assets/img/*" $ do
        route   idRoute
        compile copyFileCompiler
        match "assets/img/carousel/*" $ do
            route   idRoute
            compile copyFileCompiler





    match "assets/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler





    match "assets/js/*" $ do
        route   idRoute
        compile copyFileCompiler




    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler






    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls







    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls


    match "products/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls



    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            packages <- do
               packages <- loadAll "frontpage/packages-*.md"
               loadAndApplyTemplate "templates/frontpage/package-points.html" defaultContext `traverse` packages
            images <- (fmap (\ident -> Item ident ident) )  <$> -- Build an item to match an identifier
                      getMatches "assets/img/carousel/*" :: Compiler [Item Identifier]
--            images <- recentFirst =<< loadAll "assets/img/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    listField "packages" defaultContext (return packages) `mappend`
                    listField "images" filePathCtx (traverse filepathGrabber images) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler




--------------------------------------------------------------------------------



postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

filePathCtx :: Context String
filePathCtx =
  field "img" (\i -> return $ itemBody i)



filepathGrabber :: Item Identifier -> Compiler (Item String)
filepathGrabber ident = toString
  where
    toFilePath :: Compiler (Item (Maybe FilePath))
    toFilePath = withItemBody getRoute ident
    toString :: Compiler (Item String)
    toString = fmap itemToString toFilePath
    maybeToUrl :: Maybe FilePath -> String
    maybeToUrl = maybe "tst" toUrl
    itemToString :: (Item (Maybe FilePath)) -> Item String
    itemToString = fmap maybeToUrl


lDocumentHeadings :: Lens' Document [Heading]
lDocumentHeadings = lens documentHeadings (\d n -> d {documentHeadings=n} )


lHeadingTitle :: Lens' Heading Text
lHeadingTitle = lens title (\h t -> h {title = t})


--------------------------------------------------
-- Org mode Parsing
--------------------------------------------------

parseAsOrgMode txt = returnTemplate
  where
    returnTemplate = parseDocumentWithKeyWords txt
    parseDocumentWithKeyWords orgmodeByteString =  AttoParsec.parseOnly (parseDocument keyWords)  orgmodeByteString
    keyWords = []



retrieveDocumentTitle :: Document -> Text
retrieveDocumentTitle doc = doc ^? lDocumentHeadings . folded . lHeadingTitle & fromMaybe ""

orgmodeCompiler :: Compiler (Item String)
orgmodeCompiler = do
  itemBody' <- getResourceBody
  let
    body = Text.pack . itemBody $ itemBody'
    title = Text.unpack $ either (const "") retrieveDocumentTitle $ parseAsOrgMode body
    orgContext = constField "title" title
  applyAsTemplate orgContext itemBody'
