{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
import           Control.Lens                           hiding (Context, Level)
import qualified Data.Attoparsec.Text                   as AttoParsec
import           Data.Map                               (Map)
import           Data.Maybe                             (fromMaybe)
import           Data.Monoid                            ((<>))
import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
import qualified Data.Text.IO                           as Text
import qualified Data.Text.Lazy                         as Text.Lazy
import qualified Data.Vector                            as Vector
import           Debug.Trace                            (traceShow)
import           Elements.Compilers
import           Hakyll

import qualified Text.HTML.DOM                          as DOM
import qualified Text.XML                               as XML
import           Text.XML.Lens                          (attr, attributeIs,
                                                         attributeSatisfies,
                                                         attrs, el, entire,
                                                         name, named, nodes,
                                                         root, text, _Element)
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


 
    match "contact/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler  >>= relativizeUrls



    match "products/*.org" $ do
        route $ setExtension "html"
        compile $ productCompiler  >>= relativizeUrls

    create ["products.html"] $ do
      route idRoute
      compile $ do
        productStrings <- loadAll "products/*"  :: Compiler ([Item String])
        let productListContext = listField "products" postProductContext (return productStrings)
        makeItem (""::String)
            >>= loadAndApplyTemplate "templates/product-list.html" productListContext
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
            contacts <- loadAll "contact/*.md"
            let indexCtx =
                    constField "title"    "Home"                                        `mappend`
                    listField  "packages" defaultContext (return packages)              `mappend`
                    listField  "images"   filePathCtx (traverse filepathGrabber images) `mappend`
                    listField  "contacts" defaultContext (return contacts)              `mappend`
                    defaultContext




            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler




--------------------------------------------------------------------------------
-- |Product Context fields for things using products post processing
postProductContext :: Context String
postProductContext = productTitleField <> imageField <> synopsisField <> defaultContext
  where

    productTitleField =  field "product-title" (\istr -> (return . Text.unpack .retrieveTitle.parseDoc. itemBody) istr)
    imageField        =  field "product-image" (\istr -> (return .Text.unpack. retrieveImage .parseDoc .itemBody ) istr)
    synopsisField     =  field "product-synopsis" (\istr -> (return .Text.unpack. retrieveSynopsis .parseDoc .itemBody ) istr)

    parseDoc = DOM.parseLT . Text.Lazy.pack

--    renderText = Text.unpack.dropXMLHeader . Text.Lazy.toStrict . (XML.renderText XML.def)

    retrieveTitle is = is ^. root . entire . named "title" . text
    retrieveImage :: XML.Document -> Text
    retrieveImage is = is ^. root . entire . (attributeSatisfies "main-image" (const True)) . attr "src"

    retrieveSynopsis is = is ^. root . entire . (attributeIs "id" "synopsis") . nodes. folded. _Element.
                                        named "p" . text & takeTextAndPad
    twitterLimit = 140
    takeTextAndPad :: Text -> Text
    takeTextAndPad t = if Text.length t >= twitterLimit
                         then  Text.take twitterLimit t
                         else t <> (Text.pack . mconcat. replicate  (twitterLimit - Text.length t) $ " ")





postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext




filePathCtx :: Context String
filePathCtx =
  field "img" (\i -> return $ itemBody i)



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
renderPandocBootStrapped :: RootNodeProps
                                  -> Vector.Vector (XML.Document -> XML.Document)
                                  -> Item String
                                  -> Compiler (Item String)
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
    parseWithNewHead elemName elemAttrs elem' = (DOM.parseLT elem') & root. name .~ elemName &
                                                root. attrs .~  elemAttrs


--------------------------------------------------
-- Transformations on Elements
--------------------------------------------------

-- Image Transformation


-- |Round the edges on the main images
mainImageTransformRunner :: XML.Document -> XML.Document
mainImageTransformRunner = editAllDocument "img" imageTransform
  where
    imageTransform element' = element' & attrs . at "class" %~ addTxt "img-rounded media-object" & addId
    addId element' = element' & attrs . at "main-image" %~ addTxt ""  & attrs . at "width" %~ addTxt "100%"


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
videoTransformRunner :: XML.Document -> XML.Document
videoTransformRunner = editAllDocument "h2" h2Transform
  where
    h2Transform element' = element' & nodes . traverse . _Element . named "img"  %~ videoTransform

    videoTransform :: XML.Element -> XML.Element
    videoTransform element' = runIfVid
      where


        nameElementSource :: XML.Element
        nameElementSource = element' & name .~ "source" & attrs .~ [("src",videoSrc),("type","video/mp4")]


        rawSrc :: Text
        rawSrc = element' ^. attrs . at "src" . _Just

        videoSrc = Text.dropEnd 4 rawSrc
        imgSrc = Text.dropEnd 8 rawSrc <> ".jpg"
        runIfVid = case Text.takeEnd 8 rawSrc of
                     ".mp4.jpg" -> videoE [("id","my_video"), ("class","video-js vjs-default-skin")
                                          ,("controls",""),("preload","auto"),("poster",imgSrc)
                                          , ("width","100%" )] nameElementSource

                     _ -> element'



-- | make images responsive
imageTransformRunner :: XML.Document -> XML.Document
imageTransformRunner = editAllDocument "p" imageTransform
  where
    imageTransform element' = element' & nodes.traverse. _Element . named "img" . attrs . at "class" %~ addTxt "img-rounded img-responsive"


-- Table Transformation
-- Striped and apply classes
tableTransformRunner :: XML.Document -> XML.Document
tableTransformRunner  = editAllDocument "table" (noTouchRun tableTransform)
  where
    tableTransform element' = element' & attrs . at "class" %~ addTxt "table table-striped" &
                                         touchElement &
                                           divE [("class","row")] &
                                             divE [("class", "col-md-12")]





-- Paragraph Transformation
paragraphTransformationRunner :: XML.Document -> XML.Document
paragraphTransformationRunner = editAllDocument "p" (noTouchRun paragraphTransform)
  where
    paragraphTransform element' = divE [("class","col-md-12")] $ touchElement $ element'

-- Header 2 Transformation
-- Header 2's are Rows in the product page

h2TransformationRunner :: XML.Document -> XML.Document
h2TransformationRunner = editAllDocument "h2" (noTouchRun h2Transform)
  where
    h2Transform element' = divE [("class","row")] $ touchElement $ element'


















--------------------------------------------------
-- div selector helpers
--------------------------------------------------
-- | only run a given command if the element has not been touched


noTouchRun  :: (XML.Element -> XML.Element) -> XML.Element ->  XML.Element
noTouchRun f element'
  | isTouched element' = element'
  | otherwise = f element'

divE :: Map XML.Name Text -> XML.Element -> XML.Element
divE attrs' e = XML.Element "div" attrs' [XML.NodeElement e]

videoE :: Map XML.Name Text -> XML.Element -> XML.Element
videoE attrs' e = XML.Element "video" attrs' [XML.NodeElement e]

-- Add text to an attribute if there is no text there
-- append to the text if something already exists

addTxt :: forall r. Monoid r => r -> Maybe r -> Maybe r
addTxt txt = (maybe (Just txt)
              (\t -> Just $ t <> txt))

-- | For full recursion something has to flag to ignore this element.

touchElement :: XML.Element -> XML.Element
touchElement  elem' = elem' & attrs . at "touched" .~ Just "true"


isTouched :: XML.Element -> Bool
isTouched   elem' = elem' ^. attrs . at "touched" & (== (Just "true"))

















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
--    parseError = "perror parsing OrgDocument"
    productPage :: Item ProductPage
    productPage = either emptyProductPage id .
                     fmap splitDocumentation .
                     (parseAsOrgMode  ).
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



testPrettyPrinter :: IO Text
testPrettyPrinter = do
  (Right doc) <- parseAsOrgMode <$> Text.readFile "products/example-product.org"
  return $ orgModePrinter doc

testSplitDocs :: IO ProductPage
testSplitDocs = do
   (Right doc) <- parseAsOrgMode <$> Text.readFile "products/example-product.org"
   return $ splitDocumentation doc


