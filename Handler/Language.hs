module Handler.Language where

import Import

postLanguageR :: Handler Html
postLanguageR = do
  lang <- runInputPost $ ireq hiddenField "lang"
  setLanguage lang
  redirect HomeR