module DOM.File.FileReader.Aff
  ( readAsText
  , readAsArrayBuffer
  , readAsDataURL
  ) where

import Control.Monad.Except (runExcept)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(Left,Right), either)
import Foreign (F, Foreign, unsafeReadTagged, readString)
import Effect (Effect)
import Effect.Aff (makeAff, Aff)
import Effect.Exception (error)
import Prelude
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.File.FileReader (FileReader, result, fileReader, toEventTarget)
import Web.File.FileReader as FileReader
import Web.File.Blob (Blob)
import Web.HTML.Event.EventTypes as EventTypes

readAs :: forall a. (Foreign -> F a) -> (Blob -> FileReader -> Effect Unit) -> Blob -> Aff a
readAs readMethod getResult blob = makeAff \fun -> do
  let err = fun <<< Left
      succ = fun <<< Right
  fr <- fileReader
  let et = toEventTarget fr

  errorListener <- eventListener \_ -> err (error "error")
  loadListener <- eventListener \_ -> do
    res <- result fr
    either (\errs -> err $ error $ show errs) succ $ runExcept $ readMethod res

  addEventListener EventTypes.error errorListener false et
  addEventListener EventTypes.load loadListener false et
  getResult blob fr
  pure mempty

readAsText :: Blob -> Aff String
readAsText = readAs readString FileReader.readAsText

readAsArrayBuffer :: Blob -> Aff ArrayBuffer
readAsArrayBuffer = readAs (unsafeReadTagged "ArrayBuffer") FileReader.readAsArrayBuffer

readAsDataURL :: Blob -> Aff String
readAsDataURL = readAs readString FileReader.readAsDataURL
