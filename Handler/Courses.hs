{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Courses where

import Import

import Handler.Forms
import Database

getCoursesR :: Handler Html
getCoursesR = do
  ((_, formWidget), formEnctype) <- newCourseForm
  courses <- runDB $ selectList [] [Asc CourseName]
  defaultLayout $ do
    setTitleI MsgAddCourse
    $(widgetFile "courses")

postCoursesR :: Handler Html
postCoursesR = do
  ((result, _), _) <- newCourseForm
  formHandler result $ \course -> do
    cid <- runDB $ insert course
    logInfo "Course added"
    setMessageI MsgCourseAdded
    redirect $ CourseR cid
  redirect CoursesR