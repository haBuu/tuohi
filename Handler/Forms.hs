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
  mr <- getMessageRender
  (layoutRes, layoutView) <- mreq (selectField layouts)
    (FieldSettings (SomeMessage MsgLayout) Nothing Nothing Nothing
      [("class", "form-control")]) Nothing
  (dayRes, dayView) <- mreq dayField
    (FieldSettings (SomeMessage MsgDate) Nothing Nothing Nothing
      [("placeholder", mr MsgDate), ("class", "form-control")]) Nothing
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgCompetitionName) Nothing Nothing Nothing
      [("placeholder", mr MsgCompetitionName), ("class", "form-control")]) Nothing
  (playersRes, playersView) <- mreq intField
    (FieldSettings (SomeMessage MsgPlayerLimit) Nothing Nothing Nothing
      [("min","1"),("max", "200"), ("class", "form-control")]) (Just 54)
  let competitionRes = Competition
                        <$> layoutRes
                        <*> dayRes
                        <*> nameRes
                        <*> playersRes
                        <*> (pure Init)
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel layoutView}
          ^{fvInput layoutView}
        <div .form-group>
          <label .control-label>^{fvLabel dayView}
          ^{fvInput dayView}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <label .control-label>^{fvLabel playersView}
          ^{fvInput playersView}
        <div .form-group>
            <input type=submit .btn .btn-default .btn-block value=_{MsgAddCompetition}>
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
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgAddCourse) Nothing Nothing Nothing
      [("placeholder", mr MsgAddCourse), ("class", "form-control")]) Nothing
  let courseRes = Course <$> nameRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block value=_{MsgAddCourse}>
      |]
  return (courseRes, widget)

newSerieForm :: Html -> MForm Handler (FormResult Serie, Widget)
newSerieForm extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgSerieName) Nothing Nothing Nothing
      [("placeholder", mr MsgSerieName), ("class", "form-control")]) Nothing
  let serieRes = Serie <$> nameRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block value=_{MsgAddSerie}>
      |]
  return (serieRes, widget)

newLayoutForm :: CourseId -> Html
  -> MForm Handler (FormResult (Layout, Int), Widget)
newLayoutForm cid extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgLayoutName) Nothing Nothing Nothing
      [("placeholder", mr MsgLayoutName), ("class", "form-control")]) Nothing
  (descRes, descView) <- mreq textField
    (FieldSettings (SomeMessage MsgLayoutDesc) Nothing Nothing Nothing
      [("placeholder", mr MsgLayoutDesc), ("class", "form-control")]) Nothing
  (holesRes, holesView) <- mreq intField
    (FieldSettings (SomeMessage MsgNumberOfHoles) Nothing Nothing Nothing
      [("min","1"), ("max", "50"), ("class", "form-control")]) (Just 9)
  let layoutRes = Layout
                    <$> (pure cid)
                    <*> nameRes
                    <*> descRes
  let res = (,) <$> layoutRes <*> holesRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <label .control-label>^{fvLabel descView}
          ^{fvInput descView}
        <div .form-group>
          <label .control-label>^{fvLabel holesView}
          ^{fvInput holesView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block value=_{MsgAddLayout}>
      |]
  return (res, widget)

holesForm :: [Entity Hole] -> Html
  -> MForm Handler (FormResult [(HoleId, Int)], Widget)
holesForm holes extra = do
  mr <- getMessageRender
  -- select field for every hole
  holeFields <- forM holes $ \(Entity hid hole) ->
    mreq (selectFieldList pars) (set (mr MsgHole) (holeNumber hole)) $ Just $ holePar hole
  let (holeResults, holeViews) = unzip holeFields
  -- add holeids
  let result = sequenceA $ Import.for (Import.zip holes holeResults) $
        \((Entity hid _), res) -> (,) <$> (pure hid) <*> res
  let widget = [whamlet|
        #{extra}
        $forall holeView <- holeViews
          <div .form-group>
            <label .control-label>^{fvLabel holeView}
            ^{fvInput holeView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block value=_{MsgUpdateLayout}>
      |]
  return (result, widget)
  where
    pars :: [(Text, Int)]
    pars = [("2", 2),("3", 3),("4", 4),("5", 5),("6", 6)]
    set :: Text -> Int -> FieldSettings App
    set hole n = FieldSettings
      (SomeMessage (pack ((unpack hole) ++ " " ++ (show n))))
      Nothing Nothing Nothing [("class", "form-control")]

startCompetitionForm :: CompetitionId -> Html
  -> MForm Handler (FormResult CompetitionId, Widget)
startCompetitionForm cid extra = do
  let widget = [whamlet|
        #{extra}
        <input type=submit .btn .btn-default .btn-block value=_{MsgStartCompetition}>
      |]
  return (pure cid, widget)

nextRoundForm :: CompetitionId -> Html
  -> MForm Handler (FormResult CompetitionId, Widget)
nextRoundForm cid extra = do
  let widget = [whamlet|
        #{extra}
        <input type=submit .btn .btn-default .btn-block value=_{MsgNextRound}>
      |]
  return (pure cid, widget)

finishCompetitionForm :: CompetitionId -> Html
  -> MForm Handler (FormResult CompetitionId, Widget)
finishCompetitionForm cid extra = do
  let widget = [whamlet|
        #{extra}
        <input type=submit .btn .btn-default .btn-block value=_{MsgFinishCompetition}>
      |]
  return (pure cid, widget)

signUpForm :: CompetitionId -> Html
  -> MForm Handler (FormResult (Text, Text, D.Division, Int), Widget)
signUpForm cid extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgName) Nothing Nothing Nothing
      [("placeholder", mr MsgName), ("class", "form-control")]) Nothing
  (emailRes, emailView) <- mreq emailField
    (FieldSettings (SomeMessage MsgEmail) Nothing Nothing Nothing
      [("placeholder", mr MsgEmail), ("class", "form-control")]) Nothing
  (divisionRes, divisionView) <- mreq (radioFieldList (divisionsRender mr))
    (FieldSettings (SomeMessage MsgDivision) Nothing Nothing Nothing
      []) Nothing
  (botCheckRes, botCheckView) <- mreq (checkBotField mr)
    (FieldSettings (SomeMessage MsgBotCheckField) Nothing Nothing Nothing
      [("placeholder", mr MsgBotCheckQuestion), ("class", "form-control")]) Nothing
  let result = (,,,)
                <$> nameRes
                <*> emailRes
                <*> divisionRes
                <*> botCheckRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <label .control-label>^{fvLabel emailView}
          ^{fvInput emailView}
        <div .form-group>
          <label>^{fvLabel divisionView}
          <div .radio>
            ^{fvInput divisionView}
        <div .form-group>
          <label .control-label>^{fvLabel botCheckView}
          ^{fvInput botCheckView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block value=_{MsgSignUp}>
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
        <div .form-group>
          <label>^{fvLabel divisionView}
          <div .radio>
            ^{fvInput divisionView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block value=_{MsgSignUp}>
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
        , ("class", "form-control")
        ])
  (scoreRes, scoreView) <- mreq (selectFieldList scores) set score
  let result = Score <$> (pure rid) <*> (pure hid) <*> scoreRes
  let widget = [whamlet|
      #{extra}
      <div .form-group>
        <label .col-sm-1 .control-label>#{name}
        <div .col-sm-2 .pull-right>
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
      [("placeholder", mr MsgName), ("class", "form-control")]) (Just $ userName user)
  (emailRes, emailView) <- mreq emailField
    (FieldSettings (SomeMessage MsgEmail) Nothing Nothing Nothing
      [("placeholder", mr MsgEmail), ("class", "form-control")]) (Just $ userEmail user)
  let result = (,) <$> nameRes <*> emailRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <label .control-label>^{fvLabel emailView}
          ^{fvInput emailView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block value=_{MsgUpdate}>
      |]
  return (result, widget)

userForm :: User -> Html -> MForm Handler (FormResult (Text, Text, Bool), Widget)
userForm user extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (FieldSettings (SomeMessage MsgName) Nothing Nothing Nothing
      [("placeholder", mr MsgName), ("class", "form-control")]) (Just $ userName user)
  (emailRes, emailView) <- mreq emailField
    (FieldSettings (SomeMessage MsgEmail) Nothing Nothing Nothing
      [("placeholder", mr MsgEmail), ("class", "form-control")]) (Just $ userEmail user)
  (adminRes, adminView) <- mreq checkBoxField
    (FieldSettings "" Nothing Nothing Nothing
      []) (Just $ userAdmin user)
  let result = (,,) <$> nameRes <*> emailRes <*> adminRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .col-sm-3 .control-label>^{fvLabel nameView}
          <div .col-sm-6>
            ^{fvInput nameView}
        <div .form-group>
          <label .col-sm-3 .control-label>^{fvLabel emailView}
          <div .col-sm-6>
            ^{fvInput emailView}
        <div .form-group>
          <div .col-sm-offset-3 .col-sm-6>
            <div .checkbox>
              <label>
                ^{fvInput adminView}_{MsgAdmin}
        <div .form-group>
          <div .col-sm-offset-3 .col-sm-6>
            <input type=submit .btn .btn-default .btn-block value=_{MsgUpdate}>
      |]
  return (result, widget)

tempAuthForm :: Html -> MForm Handler (FormResult Text, Widget)
tempAuthForm extra = do
  mr <- getMessageRender
  (pwRes, pwView) <- mreq passwordField
    (FieldSettings (SomeMessage MsgPassword) Nothing Nothing Nothing
      [("placeholder", mr MsgPassword), ("class", "form-control")]) Nothing
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel pwView}
          ^{fvInput pwView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block value=_{MsgLogIn}>
      |]
  return (pwRes, widget)

notificationForm :: UserId -> UTCTime -> Html
  -> MForm Handler (FormResult Notification, Widget)
notificationForm uid time extra = do
  mr <- getMessageRender
  (contentRes, contentView) <- mreq textareaField
    (FieldSettings (SomeMessage MsgNotification) Nothing Nothing Nothing
      [("placeholder", mr MsgNotification), ("class", "form-control")]) Nothing
  let result = Notification
                <$> contentRes
                <*> pure uid
                <*> pure time
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel contentView}
          ^{fvInput contentView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block value=_{MsgAddNotification}>
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