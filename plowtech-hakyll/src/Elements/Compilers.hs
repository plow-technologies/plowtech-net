module Elements.Compilers (filepathGrabber) where
import           Hakyll


--
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
