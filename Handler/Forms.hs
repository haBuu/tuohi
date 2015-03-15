{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Forms where

import Import hiding(for)

import Yesod.Form.Bootstrap3
import qualified Data.Text as T

import Model.CompetitionState
import DivisionMessages
import qualified Handler.Division as D
import Model.Permission
import Helpers

-- form handler with default action for FormFailure and FormMissing
formHandler :: FormResult a -> (a -> Handler ()) -> Handler ()
formHandler result f =
  case result of
    FormSuccess res -> f res
    FormFailure err -> setMessageI $ MsgFormFailure $ T.concat err
    FormMissing -> setMessageI MsgFormMissing

-- default submit button
submitButton :: a -> BootstrapSubmit a
submitButton msg = BootstrapSubmit msg "btn btn-default btn-block btn-lg" []

newCompetitionForm :: UserId -> Html
  -> MForm Handler (FormResult Competition, Widget)
newCompetitionForm uid extra = do
  mr <- getMessageRender
  (layoutRes, layoutView) <- mreq (selectField layouts)
    (bfs MsgLayout) Nothing
  (dayRes, dayView) <- mreq dayField
    (withPlaceholder (mr MsgDate) $ bfs MsgDate) Nothing
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgCompetitionName) $ bfs MsgCompetitionName) Nothing
  (playersRes, playersView) <- mreq intField
    (FieldSettings (SomeMessage MsgPlayerLimit) Nothing Nothing Nothing
      [("min","1"),("max", "200"), ("class", "form-control")]) (Just 54)
  (pwRes, pwView) <- mreq textField
    (withPlaceholder (mr MsgPassword) $ bfs MsgPassword) Nothing
  (serieRes, serieView) <- mopt (selectField series)
    (bfs MsgSerie) Nothing
  let competitionRes = Competition
                        <$> (pure uid)
                        <*> layoutRes
                        <*> dayRes
                        <*> nameRes
                        <*> playersRes
                        <*> (pure Init)
                        <*> pwRes
                        <*> serieRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel layoutView}
          ^{fvInput layoutView}
        <div .form-group>
          <label .control-label>^{fvLabel dayView}
          <div .input-group .date>
            ^{fvInput dayView}
            <span .input-group-addon>
              <i .glyphicon .glyphicon-calendar>
        <div .form-group>
          <label .control-label>^{fvLabel serieView}
          ^{fvInput serieView}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <label .control-label>^{fvLabel playersView}
          ^{fvInput playersView}
        <div .form-group>
          <label .control-label>^{fvLabel pwView}
          ^{fvInput pwView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgAddCompetition}>
      |]
  return (competitionRes, widget)
  where
    -- get layouts from db
    layouts :: Handler (OptionList LayoutId)
    layouts = do
      entities <- runDB $ selectList [] [Asc LayoutName]
      optionsPairs $ for entities $
        \(Entity lid layout) -> (layoutName layout, lid)
    -- get series from db
    series :: Handler (OptionList SerieId)
    series = do
      entities <- runDB $ selectList [] [Asc SerieName]
      optionsPairs $ for entities $
        \(Entity sid serie) -> (serieName serie, sid)

editCompetitionForm :: Competition -> Html
  -> MForm Handler (FormResult Competition, Widget)
editCompetitionForm competition extra = do
  mr <- getMessageRender
  (layoutRes, layoutView) <- mreq (selectField layouts)
    (bfs MsgLayout)
    (Just $ competitionLayoutId competition)
  (dayRes, dayView) <- mreq dayField
    (withPlaceholder (mr MsgDate) $ bfs MsgDate)
    (Just $ competitionDate competition)
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgCompetitionName) $ bfs MsgCompetitionName)
    (Just $ competitionName competition)
  (playersRes, playersView) <- mreq intField
    (FieldSettings (SomeMessage MsgPlayerLimit) Nothing Nothing Nothing
      [("min","1"),("max", "200"), ("class", "form-control")])
    (Just $ competitionPlayerLimit competition)
  (pwRes, pwView) <- mreq textField
    (withPlaceholder (mr MsgPassword) $ bfs MsgPassword)
    (Just $ competitionPassword competition)
  (serieRes, serieView) <- mopt (selectField series)
    (bfs MsgSerie)
    (Just $ competitionSerieId competition)
  let competitionRes = Competition
                        <$> (pure $ competitionUserId competition)
                        <*> layoutRes
                        <*> dayRes
                        <*> nameRes
                        <*> playersRes
                        <*> (pure $ competitionState competition)
                        <*> pwRes
                        <*> serieRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel layoutView}
          ^{fvInput layoutView}
        <div .form-group>
          <label .control-label>^{fvLabel dayView}
          <div .input-group .date>
            ^{fvInput dayView}
            <span .input-group-addon>
              <i .glyphicon .glyphicon-calendar>
        <div .form-group>
          <label .control-label>^{fvLabel serieView}
          ^{fvInput serieView}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <label .control-label>^{fvLabel playersView}
          ^{fvInput playersView}
        <div .form-group>
          <label .control-label>^{fvLabel pwView}
          ^{fvInput pwView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgEditCompetition}>
      |]
  return (competitionRes, widget)
  where
    -- get layouts from db
    layouts :: Handler (OptionList LayoutId)
    layouts = do
      entities <- runDB $ selectList [] [Asc LayoutName]
      optionsPairs $ for entities $
        \(Entity lid layout) -> (layoutName layout, lid)
    -- get series from db
    series :: Handler (OptionList SerieId)
    series = do
      entities <- runDB $ selectList [] [Asc SerieName]
      optionsPairs $ for entities $
        \(Entity sid serie) -> (serieName serie, sid)

newCourseForm :: Handler ((FormResult Course, Widget), Enctype)
newCourseForm = do
  mr <- getMessageRender
  let settings = withPlaceholder (mr MsgAddCourse) $ bfs MsgAddCourse
  runFormPost $ renderBootstrap3 BootstrapBasicForm $ Course
    <$> areq textField settings Nothing
    <*  bootstrapSubmit (submitButton MsgAddCourse)

newSerieForm :: Handler ((FormResult Serie, Widget), Enctype)
newSerieForm = do
  mr <- getMessageRender
  let settings = withPlaceholder (mr MsgSerieName) $ bfs MsgSerieName
  runFormPost $ renderBootstrap3 BootstrapBasicForm $ Serie
    <$> areq textField settings Nothing
    <*  bootstrapSubmit (submitButton MsgAddSerie)

newLayoutForm :: CourseId -> Html
  -> MForm Handler (FormResult (Layout, Int), Widget)
newLayoutForm cid extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgLayoutName) $ bfs MsgLayoutName) Nothing
  (descRes, descView) <- mreq textField
    (withPlaceholder (mr MsgLayoutDesc) $ bfs MsgLayoutDesc) Nothing
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
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgAddLayout}>
      |]
  return (res, widget)

holesForm :: [Entity Hole] -> Html
  -> MForm Handler (FormResult [(HoleId, Int)], Widget)
holesForm holes extra = do
  -- select field for every hole
  holeFields <- forM holes $ \(Entity _ hole) ->
    mreq (selectFieldList pars) (set (holeNumber hole)) $ Just $ holePar hole
  let (holeResults, holeViews) = unzip holeFields
  -- add holeids
  let result = sequenceA $ for (Import.zip holes holeResults) $
        \((Entity hid _), res) -> (,) <$> (pure hid) <*> res
  let widget = [whamlet|
        #{extra}
        $forall holeView <- holeViews
          <div .form-group>
            <label .control-label>^{fvLabel holeView}
            ^{fvInput holeView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgUpdateLayout}>
      |]
  return (result, widget)
  where
    pars :: [(Text, Int)]
    pars = [("2", 2),("3", 3),("4", 4),("5", 5),("6", 6)]
    set :: Int -> FieldSettings App
    set = bfs . MsgHoleNumber

startCompetitionForm :: CompetitionId
  -> Handler ((FormResult CompetitionId, Widget), Enctype)
startCompetitionForm cid = do
  runFormPost $ renderBootstrap3 BootstrapBasicForm $ (pure cid)
    <* bootstrapSubmit (submitButton MsgStartCompetition)

nextRoundForm :: CompetitionId
  -> Handler ((FormResult CompetitionId, Widget), Enctype)
nextRoundForm cid = do
  runFormPost $ identifyForm "nextround" $
    renderBootstrap3 BootstrapBasicForm $ (pure cid)
      <* bootstrapSubmit (submitButton MsgNextRound)

finishCompetitionForm :: CompetitionId
  -> Handler ((FormResult CompetitionId, Widget), Enctype)
finishCompetitionForm cid = do
  runFormPost $ identifyForm "finish" $
    renderBootstrap3 BootstrapBasicForm $ (pure cid)
      <* bootstrapSubmit (submitButton MsgFinishCompetition)

addPlayerForm :: Html
  -> MForm Handler (FormResult (Text, Text, D.Division), Widget)
addPlayerForm extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) Nothing
  (emailRes, emailView) <- mreq emailField
    (withPlaceholder (mr MsgEmail) $ bfs MsgEmail) Nothing
  (divisionRes, divisionView) <- mreq (radioFieldList (divisionsRender mr))
    (bfs MsgDivision) Nothing
  let result = (,,)
                <$> nameRes
                <*> emailRes
                <*> divisionRes
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
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgAddPlayer}>
      |]
  return (result, widget)

signUpForm :: Html
  -> MForm Handler (FormResult (Text, Text, D.Division), Widget)
signUpForm extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) Nothing
  (emailRes, emailView) <- mreq emailField
    (withPlaceholder (mr MsgEmail) $ bfs MsgEmail) Nothing
  (divisionRes, divisionView) <- mreq (radioFieldList (divisionsRender mr))
    (FieldSettings (SomeMessage MsgDivision) Nothing Nothing Nothing
      []) Nothing
  let result = (,,)
                <$> nameRes
                <*> emailRes
                <*> divisionRes
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
        <div .margin-bottom .g-recaptcha data-sitekey="6Ldnwv4SAAAAAFJePhKQwFx7SP0tSiheSbp-7WME">
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgSignUp}>
      |]
  return (result, widget)

signUpFormLoggedIn :: User -> Html
  -> MForm Handler (FormResult (Text, Text, D.Division), Widget)
signUpFormLoggedIn user extra = do
  mr <- getMessageRender
  (divisionRes, divisionView) <- mreq (radioFieldList (divisionsRender mr))
    (FieldSettings (SomeMessage MsgDivision) Nothing Nothing Nothing
      []) Nothing
  let result = (,,)
                <$> pure (userName user)
                <*> pure (userEmail user)
                <*> divisionRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label>^{fvLabel divisionView}
          <div .radio>
            ^{fvInput divisionView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgSignUp}>
      |]
  return (result, widget)

-- helper
-- adds message renderer to divisions so that forms can display them
divisionsRender mr = map (\(d, msg) -> (mr msg, d)) divisions

scoreForm :: CompetitionId -> HoleId -> RoundId -> Text -> Maybe Int
  -> Html -> MForm Handler (FormResult Score, Widget)
scoreForm cid hid rid name score extra = do
  r <- getUrlRender
  let set = (FieldSettings "" Nothing Nothing Nothing
        [ ("data-url", r (ScoreR cid rid hid))
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

profileForm :: User
  -> Handler ((FormResult (Text, Text), Widget), Enctype)
profileForm user = do
  mr <- getMessageRender
  let settings1 = withPlaceholder (mr MsgName) $ bfs MsgName
      settings2 = withPlaceholder (mr MsgEmail) $ bfs MsgEmail
  runFormPost $ renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField settings1 (Just $ userName user)
    <*> areq emailField settings2 (Just $ userEmail user)
    <*  bootstrapSubmit (submitButton MsgUpdate)

userForm :: User -> Html
  -> MForm Handler (FormResult (Text, Text, Bool), Widget)
userForm user extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) (Just $ userName user)
  (emailRes, emailView) <- mreq emailField
    (withPlaceholder (mr MsgEmail) $ bfs MsgEmail) (Just $ userEmail user)
  (adminRes, adminView) <- mreq checkBoxField
    "" (Just $ userAdmin user)
  let result = (,,) <$> nameRes <*> emailRes <*> adminRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <label .control-label>^{fvLabel emailView}
          ^{fvInput emailView}
        <div .form-group>
          <div .checkbox>
            <label>
              ^{fvInput adminView}_{MsgAdmin}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgUpdate}>
      |]
  return (result, widget)

tempAuthForm :: Handler ((FormResult Text, Widget), Enctype)
tempAuthForm = do
  mr <- getMessageRender
  let settings = withPlaceholder (mr MsgPassword) $ bfs MsgPassword
  runFormPost $ renderBootstrap3 BootstrapBasicForm $
    areq passwordField settings Nothing
    <* bootstrapSubmit (submitButton MsgLogIn)

notificationForm :: UserId -> UTCTime
  -> Handler ((FormResult Notification, Widget), Enctype)
notificationForm uid time = do
  mr <- getMessageRender
  let settings = withPlaceholder (mr MsgNotification) $ bfs MsgNotification
  runFormPost $ renderBootstrap3 BootstrapBasicForm $ Notification
    <$> areq textareaField settings Nothing
    <*> pure uid
    <*> pure time
    <* bootstrapSubmit (submitButton MsgAddNotification)