{-# LANGUAGE NamedFieldPuns    #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens                           hiding (Context, Level)
import qualified Data.Attoparsec.Text                   as AttoParsec
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            (mappend, (<>))
import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import qualified Data.Text.IO                           as Text
import qualified Data.Text.Lazy                         as Text.Lazy

import           Data.Map                               (Map)
import qualified Data.Vector                            as Vector
import           Hakyll
import qualified Text.HTML.DOM                          as DOM
import qualified Text.XML                               as XML
import           Text.XML.Lens                          (attr, attributeIs,
                                                         attributeSatisfies,
                                                         attrs, el, entire,
                                                         name, named, nodes,
                                                         root, text, _Content,
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

    create ["products.html"] $ do
      route idRoute
      compile $ do
        productStrings <- loadAll "products/*"  :: Compiler ([Item String])
        let productListContext = listField "products" postProductContext (return productStrings)
        makeItem "" >>=
            loadAndApplyTemplate "templates/product-list.html" productListContext
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
-- |Product Context fields for things using products post processing
postProductContext = productTitleField <> imageField <> synopsisField <> defaultContext
  where

    productTitleField =  field "product-title" (\istr -> (return . Text.unpack .retrieveTitle.parseDoc. itemBody) istr)

    imageField = field "product-image" (\istr -> (return .Text.unpack. retrieveImage .parseDoc .itemBody ) istr)

    synopsisField = field "product-synopsis" (\istr -> (return .Text.unpack. retrieveSynopsis .parseDoc .itemBody ) istr)

    parseDoc = DOM.parseLT . Text.Lazy.pack

    renderText = Text.unpack.dropXMLHeader . Text.Lazy.toStrict . (XML.renderText XML.def)

    retrieveTitle is = is ^. root . entire . named "title" . text

    retrieveImage :: XML.Document -> Text
    retrieveImage is = is ^. root . entire . (attributeSatisfies "main-image" (const True)) . attr "src"


    retrieveSynopsis is = is ^. root . entire . (attributeIs "id" "synopsis") . entire .
                                        named "p" . text & Text.take twitterLimit
    twitterLimit = 140



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

lHeadingSubHeadings :: Lens' Heading [Heading]
lHeadingSubHeadings = lens subHeadings (\h t -> h {subHeadings = t})

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
                 , _productSynopsis    :: {-# UNPACK  #-} !Text
                 , _productDescription :: {-# UNPACK  #-} !Text
               }
  deriving (Show)




productContext :: Context ProductPage
productContext = field "product-title"  (\ip -> (return . itemBody . fmap ( Text.unpack  . _productTitle)) ip) <>
                  field "product-image"  (\ip -> (fmap itemBody. productImageCompiler . fmap (Text.unpack . _productImage)) ip) <>
                  field "product-synopsis" (\ip -> (fmap itemBody. productSynopsisCompiler . fmap (Text.unpack . _productSynopsis)) ip) <>
                  field "product-description" (\ip -> (fmap itemBody. productDescriptionCompiler . fmap (Text.unpack . _productDescription)) ip)
  where
    -- IMAGES
    productImageCompiler = renderPandocBootStrapped imageNodeProps [mainImageTransformRunner]
    imageNodeProps = RootNodeProps "div" []



    -- Synopsis
    productSynopsisCompiler = renderPandocBootStrapped synopsisNodeProps synopsisTransforms
    synopsisNodeProps = RootNodeProps "div"  [ ("class","col-md-12")
                                              , ("id","synopsis")]
    synopsisTransforms = []



    -- Description
    productDescriptionCompiler = renderPandocBootStrapped descriptionNodeProps descriptionTransforms
    descriptionNodeProps = RootNodeProps "div"  [ ("class","row")
                                                 , ("id","description")]

    descriptionTransforms = [ imageTransformRunner
                            , videoTransformRunner
                            , tableTransformRunner]


-- | a root node must be specified for any template compiler that is processing the Document node tree
-- the default root node is <html> and is almost never what is desired
type XMLAttrs = Map XML.Name Text
data RootNodeProps = RootNodeProps { rootName :: XML.Name, rootAttrs :: XMLAttrs }

-- | a Compiler that adds necessary bootstrap classes to pandoc parts
-- renderPandocBootStrapped :: Item String -> Compiler (Item String)
renderPandocBootStrapped  (RootNodeProps { rootName, rootAttrs}) transforms itemString = (fmap bootstrapify) <$>   renderPandoc itemString
  where
    bootstrapify :: String -> String
    bootstrapify = Text.unpack .bootstrapifyText.Text.pack
    bootstrapifyText :: Text -> Text
    bootstrapifyText = dropXMLHeader. Text.Lazy.toStrict . bootstrapifyLazyText.  Text.Lazy.fromStrict
    bootstrapifyLazyText = XML.renderText XML.def .applyTransforms . parseWithNewHead rootName rootAttrs
    -- Varous Transformations
    applyTransforms doc = Vector.foldr (\f doc'-> f doc') doc transforms
    parseWithNewHead :: XML.Name -> XMLAttrs -> Text.Lazy.Text -> XML.Document
    parseWithNewHead elemName elemAttrs elem = (DOM.parseLT elem) & root. name .~ elemName &
                                               root. attrs .~  elemAttrs


--------------------------------------------------
-- Transformations on Elements
--------------------------------------------------

-- Image Transformation


-- |Round the edges on the main images
mainImageTransformRunner = editAllDocument "img" imageTransform
  where
    imageTransform element = element & attrs . at "class" %~ addTxt "img-rounded media-object" & addId
    addId element = element & attrs . at "main-image" %~ addTxt ""


-- | video transformer

{--


<video id="my-video" class="video-js" controls preload="auto" width="640" height="264"
  poster="MY_VIDEO_POSTER.jpg" data-setup="{}">
    <source src="MY_VIDEO.mp4" type='video/mp4'>
    <source src="MY_VIDEO.webm" type='video/webm'>
    <p class="vjs-no-js">
      To view this video please enable JavaScript, and consider upgrading to a web browser that
      <a href="http://videojs.com/html5-video-support/" target="_blank">supports HTML5 video</a>
    </p>
  </video>


--}
videoTransformRunner = editAllDocument "h2" h2Transform
  where
    h2Transform element = element & nodes . traverse . _Element . named "img"  %~ videoTransform

    videoTransform :: XML.Element -> XML.Element
    videoTransform element = runIfVid
      where


        nameElementSource :: XML.Element
        nameElementSource = element & name .~ "source" & attrs .~ [("src",videoSrc),("type","video/mp4")]


        rawSrc :: Text
        rawSrc = element ^. attrs . at "src" . _Just

        videoSrc = Text.dropEnd 4 rawSrc
        imgSrc = Text.dropEnd 8 rawSrc <> ".jpg"
        runIfVid = case Text.takeEnd 8 rawSrc of
                     ".mp4.jpg" -> videoE [("id","my_video"), ("class","video-js vjs-default-skin")
                                          ,("controls",""),("preload","auto"),("poster",imgSrc)
                                          , ("width","720" ),("height","440")] nameElementSource

                     _ -> element



-- | make images responsive
imageTransformRunner = editAllDocument "p" imageTransform
  where
    imageTransform element = element & nodes.traverse. _Element . named "img" . attrs . at "class" %~ addTxt "img-rounded img-responsive"


-- Table Transformation
-- Striped and apply classes
tableTransformRunner  = editAllDocument "table" (noTouchRun tableTransform)
  where
    tableTransform element = element & attrs . at "class" %~ addTxt "table table-striped" &
                                       touchElement &
                                         divE [("class","row")] &
                                           divE [("class", "col-md-12")]





-- Paragraph Transformation
paragraphTransformationRunner = editAllDocument "p" (noTouchRun paragraphTransform)
  where
    paragraphTransform element = divE [("class","col-md-12")] $ touchElement $ element

-- Header 2 Transformation
-- Header 2's are Rows in the product page

h2TransformationRunner = editAllDocument "h2" (noTouchRun h2Transform)
  where
    h2Transform element = divE [("class","row")] $ touchElement $ element


















--------------------------------------------------
-- div selector helpers
--------------------------------------------------
-- | only run a given command if the element has not been touched


noTouchRun  :: (XML.Element -> XML.Element) -> XML.Element ->  XML.Element
noTouchRun f element
  | isTouched element = element
  | otherwise = f element

divE attrs e = XML.Element "div" attrs [XML.NodeElement e]

videoE attrs e = XML.Element "video" attrs [XML.NodeElement e]

addTxt txt = (maybe (Just txt)
              (\t -> Just $ t <> txt))

-- | For full recursion something has to flag to ignore this element.

touchElement :: XML.Element -> XML.Element
touchElement  elem = elem & attrs . at "touched" .~ Just "true"


isTouched :: XML.Element -> Bool
isTouched   elem = elem ^. attrs . at "touched" & (== (Just "true"))

















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
    finalDoc = doc & root %~ editAllElements name' f-- final edit to document after all changes



editAllElements  :: XML.Name -> (XML.Element -> XML.Element) -> XML.Element -> XML.Element
editAllElements name' f  = editSelfAndChildren
  where
    -- Because nodes can edit themselves you have to guard against repeat loops
    editSelf self =   self & runFunctionOnNameMatch
    editSelfAndChildren = editChildren editSelf . editSelf  -- for the root node
    runFunctionOnNameMatch = el name' %~ f

editChildren  :: (XML.Element -> XML.Element) -> XML.Element -> XML.Element
editChildren f self = self & nodes . traverse . _Element %~ f

-- Idiotic function to drop xml namespace because I need to get on with life
dropXMLHeader :: Text.Text -> Text.Text
dropXMLHeader t
 |Text.length t <= 38 = t
 |otherwise = Text.drop 38 t











--------------------------------------------------



productTitle :: Lens' ProductPage Text
productTitle = lens _productTitle (\p v -> p{_productTitle = v})

productImage :: Lens' ProductPage Text
productImage = lens _productImage (\p v -> p{_productImage = v})




productDescription :: Lens' ProductPage Text
productDescription = lens _productDescription (\p v -> p{_productDescription = v})









-- |Separate a Document into pieces of a Product Page
splitDocumentation :: Document -> ProductPage
splitDocumentation doc = ProductPage  (titlePortion doc )
                                        (imagePortion doc )
                                        (synopsisPortion doc )
                                        (descriptionPortion doc )
   where
     titlePortion d = retrieveDocumentTitle d
     imagePortion d = d ^? lDocumentHeadings .folded . lHeadingSection  <&> sectionParagraph & fromMaybe ""
     synopsisPortion d = d & lDocumentHeadings %~ (\lst -> (lst ^.. iix 1 ) &
                                                  traverse . lHeadingSubHeadings .~ [] ) & orgModePrinter
     descriptionPortion d = d & lDocumentHeadings %~ (\headings ->
                                                       headings ^. iix 1 . lHeadingSubHeadings )  & orgModePrinter






productCompiler :: Compiler (Item String)
productCompiler = do
  body <- getResourceBody :: Compiler (Item String)
  let maybeDoc =  (body ^. lItemBody & parseAsOrgMode . Text.pack) ^? _Right
  case maybeDoc of
      Nothing -> return $ body
      (Just doc) -> (templateApplication $ (const (splitDocumentation doc)) <$> body)
   where
      templateApplication body = loadAndApplyTemplate "templates/product.html" productContext  body





itemStringToProduct :: Item String -> Item ProductPage
itemStringToProduct str = productPage
  where
    parseError = "perror parsing OrgDocument"
    productPage :: Item ProductPage
    productPage = either emptyProductPage id .
                     fmap splitDocumentation .
                     (\str -> parseAsOrgMode  $ str).
                     Text.pack <$> str
    emptyProductPage s = ProductPage ("Error parsing Org Document  " <> Text.pack s) "" "" ""





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


