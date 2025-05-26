module FPO.Components.FileSidebar where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (head, tail, reverse, delete)
import Data.String (length, take, toCodePointArray, singleton)
import Data.String.CodePoints (CodePoint)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.File.File as F
import Web.File.File (File)
import Web.File.Url (createObjectURL)
import Unsafe.Coerce (unsafeCoerce)
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff)

type Output = Unit

type Input = Unit

data PDFFile = PDFFile { file :: File, url :: String }

instance eqPDFFile :: Eq PDFFile where
  eq (PDFFile a) (PDFFile b) = F.name a.file == F.name b.file

type State =
  { files :: Array PDFFile
  , sidebarOpen :: Boolean
  }

data Action
  = ToggleSidebar
  | FileUploaded (Maybe File)
  | DeleteFile PDFFile

fileSidebar :: forall query m. MonadAff m => H.Component query Input Output m
fileSidebar = 
  H.mkComponent
    { initialState: \_ -> { files : [], sidebarOpen : false }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.button
          [ HE.onClick \_ -> ToggleSidebar ]
          [ HH.text (if state.sidebarOpen then "⬅️ Schließen" else "📂 Dateien") ]
      , HH.h2_ [ HH.text "Datei-Upload & Vorschau" ]
      , HH.input
          [ HP.type_ HP.InputFile
          , HE.onFileUpload FileUploaded
          ]

      , if state.sidebarOpen then
          HH.div
            [ HP.style "position: fixed; left: 0; top: 50; width: 300px; height: 100vh; background: #f8f9fa; padding: 1rem; box-shadow: 2px 0 5px rgba(0,0,0,0.1);" ]
            [ HH.h3_ [ HH.text "Hochgeladene Dateien" ]
            , HH.div_
                (state.files <#> \(PDFFile entry) ->
                  HH.div_
                    [ HH.text ((abrFileName (F.name entry.file)) <> " ")
                    -- Download button
                    , HH.a
                        [ HP.href entry.url
                        , HP.download (F.name entry.file)
                        , HP.title "Herunterladen"
                        , HP.style "margin-left: 0.5rem;" ]
                        [ HH.text "⬇️" ]
                      -- Delete button
                      , HH.button
                        [ HE.onClick \_ -> DeleteFile (PDFFile entry)
                        , HP.title "Löschen"
                        , HP.style "margin-left: 0.5rem;" ]
                        [ HH.text "🗑️" ]
                    ]
                )
            ]
        else HH.text ""
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of

    ToggleSidebar ->
      H.modify_ \st -> st { sidebarOpen = not st.sidebarOpen }

    FileUploaded Nothing -> pure unit

    FileUploaded (Just file) -> do
      url <- liftEffect $ createObjectURL (unsafeCoerce file)
      H.modify_ \st -> st { files = st.files <> [ PDFFile { file, url } ] }
    
    DeleteFile pdfFile -> do 
      H.modify_ \st -> st { files = delete pdfFile st.files }

  abrFileName :: String -> String
  abrFileName name =
    if length name > 28 then 
      case getEnding $ reverse $ toCodePointArray name of
        Nothing -> name
        Just end -> take 22 name <> "..." <> end
    else name
    where
      getEnding :: Array CodePoint -> Maybe String
      getEnding [] = Just ""
      getEnding name' = do
        h <- head name'
        t  <- tail name'
        let cp = singleton h 
        if   cp == "."
        then Just "."
        else do
          ts <- getEnding t
          pure (ts <> cp)