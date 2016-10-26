module Test.Main where

import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Maybe.Trans (lift, MaybeT(MaybeT), runMaybeT)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.File.FileList (item)
import DOM.File.FileReader.Aff (readAsArrayBuffer, readAsText)
import DOM.File.Types (File, fileToBlob)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Event.EventTypes (change)
import DOM.HTML.HTMLInputElement (files)
import DOM.HTML.Types (HTMLInputElement, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(ElementId), elementToEventTarget, documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = unsafePartial $ do
  d <- document =<< window
  b <- fromJust <$> toMaybe <$> body d
  fileInput <- fromJust <$> toMaybe <$> getElementById (ElementId "fileInput") (documentToNonElementParentNode $ htmlDocumentToDocument d)
  let inputElt = unsafeCoerce fileInput :: HTMLInputElement
  let reader :: forall eff. File -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
      reader f = void $ runAff (\_ -> pure unit) (\_ -> pure unit) do
        res <- readAsText (fileToBlob f)
        res2 <- readAsArrayBuffer (fileToBlob f)
        liftEff $ log res

  let handler :: forall a eff. a -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
      handler _ = void $ runMaybeT do
        fs <- MaybeT $ toMaybe <$> files inputElt
        file <- MaybeT $ pure $ toMaybe $ item 0 fs
        lift $ reader file
  addEventListener change (eventListener handler) false (elementToEventTarget $ fileInput)
