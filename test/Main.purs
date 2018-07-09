module Test.Main where

import Control.Monad.Maybe.Trans (lift, MaybeT(MaybeT), runMaybeT)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (runAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.File.File (File, toBlob)
import Web.File.FileList (item)
import Web.File.FileReader.Aff (readAsArrayBuffer, readAsText)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (change)
import Web.HTML.HTMLDocument (toDocument, body)
import Web.HTML.HTMLInputElement (HTMLInputElement, files)
import Web.HTML.Window (document)

main :: Effect Unit
main = unsafePartial $ do
  d <- document =<< window
  b <- fromJust <$> body d
  fileInput <- fromJust <$> getElementById "fileInput" (toNonElementParentNode $ toDocument d)
  let inputElt = unsafeCoerce fileInput :: HTMLInputElement
  let reader :: File -> Effect Unit
      reader f = void $ runAff (\_ -> pure unit) do
        res <- readAsText (toBlob f)
        res2 <- readAsArrayBuffer (toBlob f)
        liftEffect $ log res

  handler <- eventListener \_ -> void $ runMaybeT do
    fs <- MaybeT $ files inputElt
    file <- MaybeT $ pure $ item 0 fs
    lift $ reader file

  addEventListener change handler false (toEventTarget $ fileInput)
