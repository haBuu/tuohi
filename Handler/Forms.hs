{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Forms where

import Import hiding(for)

import Yesod.Form.Bootstrap3
import qualified Data.Text as T

import Model.CompetitionState
import DivisionMessages
import qualified Handler.Division as D
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

competitionForm :: UserId -> Maybe Competition -> [D.Division] -> Html
  -> MForm Handler (FormResult (Competition, [D.Division]), Widget)
competitionForm uid mCompetition divisions extra = do
  mr <- getMessageRender
  (layoutRes, layoutView) <- mreq (selectField layouts)
    (bfs MsgLayout)
    (fmap competitionLayoutId mCompetition)
  (dayRes, dayView) <- mreq dayField
    (withPlaceholder (mr MsgDate) $ bfs MsgDate)
    (fmap competitionDate mCompetition)
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgCompetitionName) $ bfs MsgCompetitionName)
    (fmap competitionName mCompetition)
  (playersRes, playersView) <- mreq intField
    (FieldSettings (SomeMessage MsgPlayerLimit) Nothing Nothing Nothing
      [("min","1"),("max", "200"), ("class", "form-control")])
    (Just $ maybe 54 competitionPlayerLimit mCompetition)
  (pwRes, pwView) <- mreq textField
    (withPlaceholder (mr MsgCompetitionPassword) $ bfs MsgCompetitionPassword)
    (fmap competitionPassword mCompetition)
  (serieRes, serieView) <- mopt (selectField series)
    (bfs MsgSerie)
    (fmap competitionSerieId mCompetition)
  (divisionRes, divisionView) <- mreq (checkboxesFieldList (divisionsRender mr))
    (FieldSettings (SomeMessage MsgDivisions) Nothing Nothing Nothing
      []) (Just divisions)
  (pdgaRes, pdgaView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgPDGACompetition) Nothing Nothing Nothing
      []) (fmap competitionPdga mCompetition)
  let competitionRes = Competition
                        <$> (pure uid)
                        <*> layoutRes
                        <*> dayRes
                        <*> nameRes
                        <*> playersRes
                        <*> (pure $ maybe Init competitionState mCompetition)
                        <*> pwRes
                        <*> serieRes
                        <*> pdgaRes
  let result = (,) <$> competitionRes <*> divisionRes
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
          <label>^{fvLabel divisionView}
          <div .checkbox>
            ^{fvInput divisionView}
        <div .checkbox>
          <label>
            ^{fvInput pdgaView}^{fvLabel pdgaView}
        <div .form-group>
          $if isJust mCompetition
            <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgEditCompetition}>
          $else
            <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgNewCompetition}>
      |]
  return (result, widget)
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

addPlayerForm :: CompetitionId -> Html
  -> MForm Handler (FormResult (Text, Text, D.Division), Widget)
addPlayerForm cid extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) Nothing
  (emailRes, emailView) <- mreq emailField
    (withPlaceholder (mr MsgEmail) $ bfs MsgEmail) Nothing
  (divisionRes, divisionView) <- mreq (radioField (competitionDivisions cid))
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
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgAddPlayer}>
      |]
  return (result, widget)

signUpForm :: CompetitionId -> Html
  -> MForm Handler (FormResult (Text, Text, D.Division, Text), Widget)
signUpForm cid extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) Nothing
  (emailRes, emailView) <- mreq emailField
    (withPlaceholder (mr MsgEmail) $ bfs MsgEmail) Nothing
  (divisionRes, divisionView) <- mreq (radioField (competitionDivisions cid))
    (FieldSettings (SomeMessage MsgDivision) Nothing Nothing Nothing
      []) Nothing
  (pwRes, pwView) <- mreq textField
    (withPlaceholder (mr MsgCompetitionPassword) $ bfs MsgCompetitionPassword) Nothing
  let result = (,,,)
                <$> nameRes
                <*> emailRes
                <*> divisionRes
                <*> pwRes
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
          <label .control-label>^{fvLabel pwView}
          ^{fvInput pwView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgSignUp}>
      |]
  return (result, widget)

signUpFormLoggedIn :: CompetitionId -> User -> Html
  -> MForm Handler (FormResult (Text, Text, D.Division, Text), Widget)
signUpFormLoggedIn cid user extra = do
  mr <- getMessageRender
  (divisionRes, divisionView) <- mreq (radioField (competitionDivisions cid))
    (FieldSettings (SomeMessage MsgDivision) Nothing Nothing Nothing
      []) Nothing
  (pwRes, pwView) <- mreq textField
    (withPlaceholder (mr MsgCompetitionPassword) $ bfs MsgCompetitionPassword) Nothing
  let result = (,,,)
                <$> pure (userName user)
                <*> maybe (FormMissing) pure (userEmail user)
                <*> divisionRes
                <*> pwRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label>^{fvLabel divisionView}
          <div .radio>
            ^{fvInput divisionView}
        <div .form-group>
          <label .control-label>^{fvLabel pwView}
          ^{fvInput pwView}
        <div .form-group>
          <input type=submit .btn .btn-default .btn-block .btn-lg value=_{MsgSignUp}>
      |]
  return (result, widget)

competitionDivisions :: CompetitionId -> Handler (OptionList D.Division)
competitionDivisions cid = do
  mr <- getMessageRender
  entities <- runDB $ selectList
    [CompetitionDivisionCompetitionId ==. cid] []
  optionsPairs $ for entities $
    \(Entity _ compDiv) ->
      let
        division = competitionDivisionDivision compDiv
      in
        (mr $ divisionMsg division, division)

-- helper
-- adds message renderer to divisions so that forms can display them
divisionsRender :: (AppMessage -> Text) -> [(Text, D.Division)]
divisionsRender mr = map (\(d, msg) -> (mr msg, d)) allDivisions

scoreEditForm :: CompetitionId -> HoleId -> RoundId -> Int -> Maybe Int
  -> Html -> MForm Handler (FormResult Score, Widget)
scoreEditForm cid hid rid hole mScore extra = do
  r <- getUrlRender
  let set = (FieldSettings "" Nothing Nothing Nothing
        [ ("data-url", r (ScoreEditR cid rid hid))
        , ("class", "form-control")
        ])
  (scoreRes, scoreView) <- mreq (checkScore $ selectFieldList scores)
    set mScore
  let result = Score <$> (pure rid) <*> (pure hid) <*> scoreRes
  let widget = [whamlet|
      #{extra}
      <div .form-group>
        <label .col-sm-1 .control-label>_{MsgHole} #{show hole}
        <div .col-sm-2 .pull-right>
          ^{fvInput scoreView}
    |]
  return (result, widget)
  where
    scores :: [(Text, Int)]
    scores = ("#", 0) : [(pack (show i), i) | i <- [1..99]]

checkScore :: Field Handler Int -> Field Handler Int
checkScore field = checkBool
  (\v -> v >= 0 && v < 99) MsgScoreRangeError field

profileForm :: User
  -> Handler ((FormResult (Text, Text), Widget), Enctype)
profileForm user = do
  mr <- getMessageRender
  let settings1 = withPlaceholder (mr MsgName) $ bfs MsgName
      settings2 = withPlaceholder (mr MsgEmail) $ bfs MsgEmail
  runFormPost $ renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField settings1 (Just $ userName user)
    <*> areq emailField settings2 (userEmail user)
    <*  bootstrapSubmit (submitButton MsgUpdate)

userForm :: User -> Html
  -> MForm Handler (FormResult (Text, Text, Bool), Widget)
userForm user extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) (Just $ userName user)
  (emailRes, emailView) <- mreq emailField
    (withPlaceholder (mr MsgEmail) $ bfs MsgEmail) (userEmail user)
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

competitionAuthForm :: Handler ((FormResult Text, Widget), Enctype)
competitionAuthForm = do
  mr <- getMessageRender
  let settings = withPlaceholder (mr MsgCompetitionPassword) $ bfs MsgCompetitionPassword
  runFormPost $ renderBootstrap3 BootstrapBasicForm $
    areq textField settings Nothing
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

importPlayersForm :: Handler ((FormResult Textarea, Widget), Enctype)
importPlayersForm = do
  mr <- getMessageRender
  let settings = withRows "30" $ withPlaceholder (mr MsgAddPlayers) $ bfs MsgAddPlayers
  runFormPost $ renderBootstrap3 BootstrapBasicForm $
    areq textareaField settings Nothing
    <* bootstrapSubmit (submitButton MsgAddPlayers)

withRows :: Text -> FieldSettings site -> FieldSettings site
withRows n fs = fs { fsAttrs = newAttrs }
  where newAttrs = ("rows", n) : fsAttrs fs