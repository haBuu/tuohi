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
  formHandler result $ \(layout, holeCount) -> do
    lid <- runDB $ insert layout
    -- insert given number of holes with par set to 3
    let holes = for [1..holeCount] $ \n -> Hole lid n 3
    void $ runDB $ insertMany holes
    setMessageI MsgLayoutAdded
  redirect $ CourseR cid