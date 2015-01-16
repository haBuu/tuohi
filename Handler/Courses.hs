{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Courses where

import Import

import Handler.Forms
import Database

getCoursesR :: Handler Html
getCoursesR = do
  ((_, formWidget), formEnctype) <- runFormPost newCourseForm
  courses <- runDB $ selectList [] [Asc CourseName]
  muser <- maybeAuthUser
  defaultLayout $ do
    setTitle "Add new course"
    mmsg <- getMessage
    let headerWidget = $(widgetFile "header")
    $(widgetFile "courses")

postCoursesR :: Handler Html
postCoursesR = do
  ((result, _), _) <- runFormPost newCourseForm
  formHandler result $ \course -> do
    runDB $ insert_ course
    setMessage "Rata lisättiin onnistuneesti"
  redirect CoursesR