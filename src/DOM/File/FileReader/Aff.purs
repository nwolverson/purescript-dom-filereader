module DOM.File.FileReader.Aff
  ( readAsText
  , readAsArrayBuffer
  , readAsDataURL
  ) where

import Prelude
import DOM.File.FileReader as FileReader
import DOM.HTML.Event.EventTypes as EventTypes
import Control.Monad.Aff (makeAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.File.FileReader (result, fileReader)
import DOM.File.Types (Blob, FileReader, fileReaderToEventTarget)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (either)
import Data.Foreign (F, Foreign, unsafeReadTagged, readString)

readAs :: forall eff a. (Foreign -> F a) -> (Blob -> FileReader -> Eff (dom :: DOM | eff) Unit) -> Blob -> Aff (dom :: DOM | eff) a
readAs readMethod getResult blob = makeAff \err succ -> do
  fr <- fileReader
  let et = fileReaderToEventTarget fr
  addEventListener EventTypes.error (eventListener \_ -> err (error "error")) false et
  addEventListener EventTypes.load (eventListener \_ -> do
      res <- result fr
      either (const $ err $ error "Result was of incorrect type") succ $ runExcept $ readMethod res
    ) false et
  getResult blob fr

readAsText :: forall eff. Blob -> Aff (dom :: DOM | eff) String
readAsText = readAs readString FileReader.readAsText

readAsArrayBuffer :: forall eff. Blob -> Aff (dom :: DOM | eff) ArrayBuffer
readAsArrayBuffer = readAs (unsafeReadTagged "ArrayBuffer") FileReader.readAsArrayBuffer

readAsDataURL :: forall eff. Blob -> Aff (dom :: DOM | eff) String
readAsDataURL = readAs readString FileReader.readAsDataURL
