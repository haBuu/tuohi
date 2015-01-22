{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Forms where

import Import

import Data.Time(Day, UTCTime, getCurrentTime)
import Yesod.Form.Jquery
import Data.Text hiding(count, head)
import qualified Data.Text.Read
import Control.Monad(forM, liftM)
import Data.Traversable(sequenceA)
import qualified Database.Esqueleto as E

import Handler.CompetitionState
import DivisionMessages
import qualified Handler.Division as D

-- form handler with default action for FormFailure and FormMissing
formHandler :: FormResult a -> (a -> Handler ()) -> Handler ()
formHandler result f =
  case result of
    FormSuccess res -> f res
    FormFailure err -> setMessageI $ MsgFormFailure $
      Data.Text.concat err
    FormMissing -> setMessageI MsgFormMissing

newCompetitionForm :: Html
  -> MForm Handler (FormResult Competition, Widget)
newCompetitionForm extra = do
  mr <- lift getMessageRender
  (layoutRes, layoutView) <- mreq (selectField layouts)
    (FieldSettings (SomeMessage MsgLayout) Nothing Nothing Nothing
      []) Nothing
  (dayRes, dayView) <- mreq dayField
    (FieldSettings (SomeMessage MsgDate) Nothing Nothing Nothing
      [ ("placeholder", mr MsgDate)
      , ("data-role","date"),("readonly","true")]) Nothing
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgCompetitionName) Nothing Nothing Nothing
      [("placeholder", mr MsgCompetitionName)]) Nothing
  (playersRes, playersView) <- mreq rangeField
    (FieldSettings (SomeMessage MsgPlayerLimit) Nothing Nothing Nothing
      [("min","1"),("max", "200")]) (Just 54)
  let competitionRes = Competition
                        <$> layoutRes
                        <*> dayRes
                        <*> nameRes
                        <*> playersRes
                        <*> (pure Init)
  let widget = [whamlet|
        #{extra}
        <ul data-role="listview" data-inset="true">
          <li .ui-field-contain>
            <label>^{fvLabel layoutView}
            ^{fvInput layoutView}
          <li .ui-field-contain>
            <label>^{fvLabel dayView}
            ^{fvInput dayView}
          <li .ui-field-contain>
            <label>^{fvLabel nameView}
            ^{fvInput nameView}
          <li .ui-field-contain>
            <label>^{fvLabel playersView}
            ^{fvInput playersView}
          <li .ui-field-contain>
            <input type=submit value=_{MsgAddCompetition}>
      |]
  return (competitionRes, widget)
  where
    -- get layouts from db
    layouts :: Handler (OptionList LayoutId)
    layouts = do
      entities <- runDB $ selectList [] [Asc LayoutName]
      optionsPairs $ for entities $
        \(Entity lid layout) -> (layoutName layout, lid)

newCourseForm :: Html -> MForm Handler (FormResult Course, Widget)
newCourseForm extra = do
  mr <- lift getMessageRender
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgAddCourse) Nothing Nothing Nothing
      [("placeholder", mr MsgAddCourse)]) Nothing
  let courseRes = Course <$> nameRes
  let widget = [whamlet|
        #{extra}
        <ul data-role="listview" data-inset="true">
          <li .ui-field-contain>
            <label>^{fvLabel nameView}
            ^{fvInput nameView}
          <li .ui-field-contain>
            <input type=submit value=_{MsgAddCourse}>
      |]
  return (courseRes, widget)

newSerieForm :: Html -> MForm Handler (FormResult Serie, Widget)
newSerieForm extra = do
  mr <- lift getMessageRender
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgSerieName) Nothing Nothing Nothing
      [("placeholder", mr MsgSerieName)]) Nothing
  let serieRes = Serie <$> nameRes
  let widget = [whamlet|
        #{extra}
        <ul data-role="listview" data-inset="true">
          <li .ui-field-contain>
            <label>^{fvLabel nameView}
            ^{fvInput nameView}
          <li .ui-field-contain>
            <input type=submit value=_{MsgAddSerie}>
      |]
  return (serieRes, widget)

newLayoutForm :: CourseId -> Html
  -> MForm Handler (FormResult (Layout, Int), Widget)
newLayoutForm cid extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgLayoutName) Nothing Nothing Nothing
      [("placeholder", mr MsgLayoutName)]) Nothing
  (descRes, descView) <- mreq textField
    (FieldSettings (SomeMessage MsgLayoutDesc) Nothing Nothing Nothing
      [("placeholder", mr MsgLayoutDesc)]) Nothing
  (holesRes, holesView) <- mreq rangeField
    (FieldSettings (SomeMessage MsgNumberOfHoles) Nothing Nothing Nothing
      [("min","1"),("max", "50")]) (Just 9)
  let layoutRes = Layout
                    <$> (pure cid)
                    <*> nameRes
                    <*> descRes
  let res = (,) <$> layoutRes <*> holesRes
  let widget = [whamlet|
        #{extra}
        <ul data-role="listview" data-inset="true">
          <li .ui-field-contain>
            <label>^{fvLabel nameView}
            ^{fvInput nameView}
          <li .ui-field-contain>
            <label>^{fvLabel descView}
            ^{fvInput descView}
          <li .ui-field-contain>
            <label>^{fvLabel holesView}
            ^{fvInput holesView}
          <li .ui-field-contain>
            <input type=submit value=_{MsgAddLayout}>
      |]
  return (res, widget)

holesForm :: [Entity Hole] -> Html
  -> MForm Handler (FormResult [(HoleId, Int)], Widget)
holesForm holes extra = do
  -- select field for every hole
  holeFields <- forM holes $ \(Entity hid hole) ->
    mreq (selectFieldList pars) set $ Just $ holePar hole
  let (holeResults, holeViews) = unzip holeFields
  -- add holeids
  let result = sequenceA $ Import.for (Import.zip holes holeResults) $
        \((Entity hid _), res) -> (,) <$> (pure hid) <*> res
  let widget = [whamlet|
        #{extra}
        <ul data-role="listview" data-inset="true">
          <li .ui-field-contain>
            <label>_{MsgPars}
            <fieldset data-role="controlgroup" data-type="horizontal">
              $forall holeView <- holeViews
                ^{fvInput holeView}
          <li .ui-field-contain>
            <input type=submit value=_{MsgUpdateLayout}>
      |]
  return (result, widget)
  where
    pars :: [(Text, Int)]
    pars = [("2", 2),("3", 3),("4", 4),("5", 5),("6", 6)]
    set :: FieldSettings App
    set = FieldSettings (SomeMessage MsgPars)
      Nothing Nothing Nothing [("data-icon","false")]

startCompetitionForm :: CompetitionId -> Html
  -> MForm Handler (FormResult CompetitionId, Widget)
startCompetitionForm cid extra = do
  let widget = [whamlet|
        #{extra}
        <input type=submit value=_{MsgStartCompetition}>
      |]
  return (pure cid, widget)

nextRoundForm :: CompetitionId -> Html
  -> MForm Handler (FormResult CompetitionId, Widget)
nextRoundForm cid extra = do
  let widget = [whamlet|
        #{extra}
        <input type=submit value=_{MsgNextRound}>
      |]
  return (pure cid, widget)

finishCompetitionForm :: CompetitionId -> Html
  -> MForm Handler (FormResult CompetitionId, Widget)
finishCompetitionForm cid extra = do
  let widget = [whamlet|
        #{extra}
        <input type=submit value=_{MsgFinishCompetition}>
      |]
  return (pure cid, widget)

signUpForm :: CompetitionId -> Html
  -> MForm Handler (FormResult (Text, Text, D.Division, Int), Widget)
signUpForm cid extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgName) Nothing Nothing Nothing
      [("placeholder", mr MsgName)]) Nothing
  (emailRes, emailView) <- mreq emailField
    (FieldSettings (SomeMessage MsgEmail) Nothing Nothing Nothing
      [("placeholder", mr MsgEmail)]) Nothing
  (divisionRes, divisionView) <- mreq (radioFieldList (divisionsRender mr))
    (FieldSettings (SomeMessage MsgDivision) Nothing Nothing Nothing
      []) Nothing
  (botCheckRes, botCheckView) <- mreq (checkBotField mr)
    (FieldSettings (SomeMessage MsgBotCheckField) Nothing Nothing Nothing
      [("placeholder", mr MsgBotCheckQuestion)]) Nothing
  let result = (,,,)
                <$> nameRes
                <*> emailRes
                <*> divisionRes
                <*> botCheckRes
  let widget = [whamlet|
        #{extra}
        <label>^{fvLabel nameView}
        ^{fvInput nameView}
        <label>^{fvLabel emailView}
        ^{fvInput emailView}
        <label>^{fvLabel divisionView}
        ^{fvInput divisionView}
        <label>^{fvLabel botCheckView}
        ^{fvInput botCheckView}
        <input type=submit value=_{MsgSignUp}>
      |]
  return (result, widget)
  where
    checkBotField mr = checkBool (==5) (mr MsgWrongAnswer) intField

signUpFormLoggedIn :: CompetitionId -> User -> Html
  -> MForm Handler (FormResult (Text, Text, D.Division, Int), Widget)
signUpFormLoggedIn cid user extra = do
  mr <- getMessageRender
  (divisionRes, divisionView) <- mreq (radioFieldList (divisionsRender mr))
    (FieldSettings (SomeMessage MsgDivision) Nothing Nothing Nothing
      []) Nothing
  let name = userName user
      email = userEmail user
  let result = (,,,)
                <$> pure name
                <*> pure email
                <*> divisionRes
                <*> (pure 0)
  let widget = [whamlet|
        #{extra}
        <label>^{fvLabel divisionView}
        ^{fvInput divisionView}
        <input type=submit value=_{MsgSignUp}>
      |]
  return (result, widget)

-- helper
-- adds message renderer to divisions so that forms can display them
divisionsRender mr = Import.map (\(d, msg) -> (mr msg, d)) divisions

scoreForm :: HoleId -> RoundId -> Text -> Maybe Int
   -> Html -> MForm Handler (FormResult Score, Widget)
scoreForm hid rid name score extra = do
  r <- getUrlRender
  let set = (FieldSettings "" Nothing Nothing Nothing
        [ ("data-icon","false")
        , ("data-mini","true")
        , ("data-inline","true")
        , ("data-url", r (ScoreR rid hid))
        ])
  (scoreRes, scoreView) <- mreq (selectFieldList scores) set score
  let result = Score <$> (pure rid) <*> (pure hid) <*> scoreRes
  let widget = [whamlet|
      #{extra}
      <fieldset .ui-grid-a>
        <div .ui-block-a>
          <p>#{name}
        <div .ui-block-b>
          ^{fvInput scoreView}
    |]
  return (result, widget)
  where
    scores :: [(Text, Int)]
    scores = ("#", 0) : [(pack (show i), i) | i <- [1..99]]

profileForm :: User -> Html -> MForm Handler (FormResult (Text, Text), Widget)
profileForm user extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgName) Nothing Nothing Nothing
      [("placeholder", mr MsgName)]) (Just $ userName user)
  (emailRes, emailView) <- mreq emailField
    (FieldSettings (SomeMessage MsgEmail) Nothing Nothing Nothing
      [("placeholder", mr MsgEmail)]) (Just $ userEmail user)
  let result = (,) <$> nameRes <*> emailRes
  let widget = [whamlet|
        #{extra}
        <label>^{fvLabel nameView}
        ^{fvInput nameView}
        <label>^{fvLabel emailView}
        ^{fvInput emailView}
        <input type=submit value=_{MsgUpdate}>
      |]
  return (result, widget)

userForm :: User -> Html -> MForm Handler (FormResult Bool, Widget)
userForm user extra = do
  (adminRes, adminView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgAdmin) Nothing Nothing Nothing
      []) (Just $ userAdmin user)
  let widget = [whamlet|
        #{extra}
        <ul data-role="listview" data-inset="true">
          <li>_{MsgName}: #{userName user}
          <li>_{MsgEmail}: #{userEmail user}
          <li .ui-field-contain>
            <label>^{fvLabel adminView}
              ^{fvInput adminView}
          <li .ui-field-contain>
            <input type=submit value=_{MsgUpdate}>
      |]
  return (adminRes, widget)

tempAuthForm :: Html -> MForm Handler (FormResult Text, Widget)
tempAuthForm extra = do
  mr <- getMessageRender
  (pwRes, pwView) <- mreq passwordField
    (FieldSettings (SomeMessage MsgPassword) Nothing Nothing Nothing
      [("placeholder", mr MsgPassword)]) Nothing
  let widget = [whamlet|
        #{extra}
        <label>^{fvLabel pwView}
        ^{fvInput pwView}
        <input type=submit value=_{MsgLogIn}>
      |]
  return (pwRes, widget)

notificationForm :: UserId -> UTCTime -> Html
  -> MForm Handler (FormResult Notification, Widget)
notificationForm uid time extra = do
  mr <- getMessageRender
  (contentRes, contentView) <- mreq textareaField
    (FieldSettings (SomeMessage MsgNotification) Nothing Nothing Nothing
      [("placeholder", mr MsgNotification)]) Nothing
  let result = Notification
                <$> contentRes
                <*> pure uid
                <*> pure time
  let widget = [whamlet|
        #{extra}
        <ul data-role="listview" data-inset="true">
          <li .ui-field-contain>
            <label>^{fvLabel contentView}
            ^{fvInput contentView}
          <li .ui-field-contain>
            <input type=submit value=_{MsgAddNotification}>
      |]
  return (result, widget)

-- form helpers

-- this is a copy from intField but type is changed to range from number
rangeField :: (Monad m, Integral i, RenderMessage (HandlerSite m) FormMessage) => Field m i
rangeField = Field
  { fieldParse = parseHelper $ \s ->
      case Data.Text.Read.signed Data.Text.Read.decimal s of
        Right (a, "") -> Right a
        _ -> Left $ MsgInvalidInteger s

  , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="range" step=1 :isReq:required="" value="#{showVal val}">
|]
  , fieldEnctype = UrlEncoded
  }
  where
    showVal = either id (pack . showI)
    showI x = show (fromIntegral x :: Integer)