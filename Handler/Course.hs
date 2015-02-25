{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Course where

import Import hiding(for)

import Handler.Forms
import Helpers

getCourseR :: CourseId -> Handler Html
getCourseR cid = do
  ((_, formWidget), formEnctype) <- runFormPost $ newLayoutForm cid
  layouts <- runDB $ selectList [LayoutCourseId ==. cid] [Asc LayoutName]
  defaultLayout $ do
    setTitleI MsgLayouts
    $(widgetFile "course")

postCourseR :: CourseId -> Handler Html
postCourseR cid = do
  ((result, _), _) <- runFormPost $ newLayoutForm cid
  formHandler result $ \(layout, holeCount) ->
    runDB $ do
      mlid <- insertUnique layout
      case mlid of
        Just lid -> do
          -- insert given number of holes with par set to 3
          let holes = for [1..holeCount] $ \n -> Hole lid n 3
          insertMany_ holes
          setMessageI MsgLayoutAdded
          redirect $ LayoutR cid lid
        Nothing ->
          setMessageI MsgLayoutExists
  redirect $ CourseR cid