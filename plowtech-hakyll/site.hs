--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens                           hiding (Context, Level)
import qualified Data.Attoparsec.Text                   as AttoParsec
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            (mappend, (<>))
import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types
import           Data.OrgMode.Parse.Types
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import qualified Data.Text.IO                           as Text
import qualified Data.Text.Lazy                         as Text.Lazy

import qualified Data.Vector                            as Vector
import           Debug.Trace
import           Hakyll
import qualified Text.HTML.DOM                          as DOM
import qualified Text.XML                               as XML
import           Text.XML.Lens                          (attrs, el, named,
                                                         nodes, root, text,
                                                         _Element)

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
        compile $ productCompiler  >>= relativizeUrls

    -- create "products.html" $ do
    --   route $ setExtension "html"
    --   compile $ do
    --     productStrings <- recentFirst =<< loadAll "products/*"

    --     let productsCtx = listField "products" productContext (return productPage) :: Context String
    --         productDocument :: [Item (Either String Document)]
    --         productDocument = (productStrings & folded.lItemBody %~ (Text.pack))
    --         productPage :: [Item ProductPage]
    --         productPage = _ -- productDocument & traverse . _Right %~ splitDocumentation
    --         productPages = _ -- (productStrings & folded.lItemBody %~ (parseAsOrgMode . Text.pack))
    --     loadAndApplyTemplate "templates/product-list.html" productsCtx productPages
    --     >>= relativizeUrls

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

            packages <- do
               packages <- loadAll "frontpage/packages-*.md"
               loadAndApplyTemplate "templates/frontpage/package-points.html" defaultContext `traverse` packages
            images <- (fmap (\ident -> Item ident ident) )  <$> -- Build an item to match an identifier
                      getMatches "assets/img/carousel/*" :: Compiler [Item Identifier]
--            images <- recentFirst =<< loadAll "assets/img/*"
            let indexCtx =
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

lItemBody :: Lens' (Item a) a
lItemBody = lens itemBody (\i v -> i{itemBody= v})

lHeadingTitle :: Lens' Heading Text
lHeadingTitle = lens title (\h t -> h {title = t})

lHeadingLevel :: Lens' Heading Level
lHeadingLevel = lens level (\h t -> h {level = t})


lHeadingSection :: Lens' Heading Section
lHeadingSection = lens section (\h t -> h {section = t})

--------------------------------------------------
-- Org mode Parsing
--------------------------------------------------

parseAsOrgMode :: Text -> Either String Document
parseAsOrgMode txt = returnTemplate
  where
    returnTemplate = parseDocumentWithKeyWords txt
    parseDocumentWithKeyWords orgmodeByteString =  AttoParsec.parseOnly (parseDocument keyWords)  orgmodeByteString
    keyWords = []



retrieveDocumentTitle :: Document -> Text
retrieveDocumentTitle doc = doc ^? lDocumentHeadings . folded . lHeadingTitle & fromMaybe ""


--    title = Text.unpack $ either (const "") retrieveDocumentTitle $ parseAsOrgMode body
--    orgContext = constField "product-title" title
--  applyAsTemplate orgContext itemBody'
--  rslt <- pandocCompiler
--  return (orgContext , rslt)

isoLevel :: Iso' Level Int
isoLevel = iso (\(Level a) -> a) Level









--------------------------------------------------
-- | Product Definition
--------------------------------------------------

-- | To properly print a product out on a product page, we want a type that defines its various parts.  This allows rendering
-- to take place more smoothly

data ProductPage = ProductPage {
                   _productTitle       :: {-# UNPACK  #-} !Text
                 , _productImage       :: {-# UNPACK  #-} !Text
                 , _productDescription :: {-# UNPACK  #-} !Text
               }
  deriving (Show)




productContext :: Context ProductPage
productContext = field "product-title"  (\ip -> (return . itemBody . fmap (Text.unpack . _productTitle)) ip) <>
                 field "product-image"  (\ip -> (fmap itemBody. productImageCompiler . fmap (Text.unpack . _productImage)) ip) <>
                 field "product-description" (\ip -> (fmap itemBody. productDescriptionCompiler . fmap (Text.unpack . _productDescription)) ip)
  where
    productImageCompiler = renderPandocBootStrapped [mainImageTransformRunner]
    productDescriptionCompiler = renderPandocBootStrapped [imageTransformRunner, tableTransformRunner]

-- | a Compiler that adds necessary bootstrap classes to pandoc parts
-- renderPandocBootStrapped :: Item String -> Compiler (Item String)
renderPandocBootStrapped transforms itemString = (fmap bootstrapify) <$>   renderPandoc itemString
  where
    bootstrapify :: String -> String
    bootstrapify = Text.unpack .bootstrapifyText.Text.pack
    bootstrapifyText :: Text -> Text
    bootstrapifyText = dropXMLHeader. Text.Lazy.toStrict . bootstrapifyLazyText.  Text.Lazy.fromStrict
    bootstrapifyLazyText = XML.renderText XML.def .applyTransforms . DOM.parseLT


    -- Varous Transformations

    applyTransforms doc = Vector.foldr (\f doc'-> f doc') doc transforms



--------------------------------------------------
-- Transformations on Elements
--------------------------------------------------

-- Image Transformation

mainImageTransformRunner = editAllDocument "img" imageTransform
  where
    imageTransform element = element & attrs . at "class" %~ addTxt "img-rounded"



imageTransformRunner = editAllDocument "img" imageTransform
  where
    imageTransform element = element & attrs . at "class" %~ addTxt "img-rounded img-responsive"



-- Table Transformation
tableTransformRunner  = editAllDocument "table" tableTransform
  where
    tableTransform element = element & attrs . at "class" %~ addTxt "table table-bordered"


addTxt txt = (maybe (Just txt)
              (\t -> Just $ t <> txt))











--------------------------------------------------



productTitle :: Lens' ProductPage Text
productTitle = lens _productTitle (\p v -> p{_productTitle = v})

productImage :: Lens' ProductPage Text
productImage = lens _productImage (\p v -> p{_productImage = v})

productDescription :: Lens' ProductPage Text
productDescription = lens _productDescription (\p v -> p{_productDescription = v})


splitDocumentation :: Document -> ProductPage
splitDocumentation doc = ProductPage  (titlePortion doc )
                                      (imagePortion doc )
                                      (descriptionPortion doc )
   where
     titlePortion d = retrieveDocumentTitle d
     imagePortion d = d ^? lDocumentHeadings .folded . lHeadingSection  <&> sectionParagraph & fromMaybe ""
     descriptionPortion d = d & lDocumentHeadings %~ drop 1 & orgModePrinter







productCompiler :: Compiler (Item String)
productCompiler = do
  body <- getResourceBody :: Compiler (Item String)
  let maybeDoc =  (body ^. lItemBody & parseAsOrgMode . Text.pack) ^? _Right
  case maybeDoc of
      Nothing -> return $ body
      (Just doc) -> (templateApplication $ (const (splitDocumentation doc)) <$> body)
   where
      templateApplication body = loadAndApplyTemplate "templates/product.html" productContext  body








-- | Use this to print back multiple Documents after parsing and sorting

-- | Pretty Printer for headings and body
orgModePrinter :: Document -> Text
orgModePrinter doc =  ( printHeadingVector . Vector.fromList . documentHeadings) doc


printHeadingVector :: Vector.Vector Heading -> Text
printHeadingVector vec = Vector.foldr' printDocument "" vec
  where
    printDocument heading txt = printHeader heading <>
                                (printHeadingVector . Vector.fromList. subHeadings) heading <>
                                txt
    printHeader heading  = levelPrinter heading <>" " <> title heading <> "\n" <> (sectionParagraph . section) heading



    levelPrinter heading = Text.replicate (heading^.lHeadingLevel.isoLevel) "*"


testPrettyPrinter = do
  (Right doc) <- parseAsOrgMode <$> Text.readFile "products/example-product.org"
  return $ orgModePrinter doc

testSplitDocs = do
   (Right doc) <- parseAsOrgMode <$> Text.readFile "products/example-product.org"
   return $ splitDocumentation doc


--------------------------------------------------
-- HTML Parsing Helpers
--------------------------------------------------

-- | Find elements by name and change them
-- Name is the name of an element, the 'div' in <div>
-- >>> doc <- DOM.readFile "example.html"
-- >>> editAllDocument "img" addSpecialClassToImages doc
editAllDocument :: XML.Name -> (XML.Element -> XML.Element) -> XML.Document -> XML.Document
editAllDocument name' f doc = finalDoc
  where
    finalDoc = doc & root %~ editSelfAndChildren -- final edit to document after all changes
    editSelf self = self &  el name' %~ f
    editChildren self = self & nodes . traverse . _Element %~ (editChildren . editSelf)
    editSelfAndChildren = editChildren.editSelf  -- for the root node

editAllElements name' f elem = editSelfAndChildren
  where
    editSelf self = self &  el name' %~ f
    editChildren self = self & nodes . traverse . _Element %~ (editChildren . editSelf)
    editSelfAndChildren = editChildren.editSelf  -- for the root node


-- Idiotic function to drop xml namespace because I need to get on with life
dropXMLHeader :: Text.Text -> Text.Text
dropXMLHeader t
 |Text.length t <= 38 = t
 |otherwise = Text.drop 38 t


