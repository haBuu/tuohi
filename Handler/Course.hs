{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Course where

import Import

import Handler.Forms
import Database

getCourseR :: CourseId -> Handler Html
getCourseR cid = do
  ((_, formWidget), formEnctype) <- runFormPost $ newLayoutForm cid
  layouts <- runDB $ selectList [LayoutCourseId ==. cid] [Asc LayoutName]
  muser <- maybeAuthUser
  defaultLayout $ do
    setTitle "Add new layout"
    mmsg <- getMessage
    let headerWidget = $(widgetFile "header")
    $(widgetFile "course")

postCourseR :: CourseId -> Handler Html
postCourseR cid = do
  ((result, _), _) <- runFormPost $ newLayoutForm cid
  formHandler result $ \(layout, holeCount) -> do
    lid <- runDB $ insert layout
    -- insert given number of holes with par set to 3
    let holes = for [1..holeCount] $ \n -> Hole lid n 3
    _ <- runDB $ insertMany holes
    setMessage "Layout lisättiin onnistuneesti"
  redirect $ CourseR cid