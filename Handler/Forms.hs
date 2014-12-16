{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Forms where

import Import
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

import Data.Time(Day)
import Yesod.Form.Jquery
import Data.Text hiding(count)
import qualified Data.Text.Read
import Control.Monad(forM)

import Handler.CompetitionState
import qualified Handler.Division as D

import qualified Database.Esqueleto as E

newCompetitionForm :: Html -> MForm Handler (FormResult Competition, Widget)
newCompetitionForm extra = do
  (layoutRes, layoutView) <- mreq (selectField layouts)
    "Layout" Nothing
  (dayRes, dayView) <- mreq dayField
    (FieldSettings "Päivämäärä" Nothing Nothing Nothing
      [("placeholder","Päivämäärä"),("data-role","date"),("readonly","true")]) Nothing
  (nameRes, nameView) <- mreq textField
    (FieldSettings "Kisan nimi" Nothing Nothing Nothing
      [("placeholder","Kisan nimi")]) Nothing
  (playersRes, playersView) <- mreq rangeField
    (FieldSettings "Pelaajamäärä" Nothing Nothing Nothing
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
            <input type=submit value="Lisää kisa">
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
  (nameRes, nameView) <- mreq textField
    (FieldSettings "Radan nimi" Nothing Nothing Nothing
      [("placeholder","Radan nimi")]) Nothing
  let courseRes = Course <$> nameRes
  let widget = [whamlet|
        #{extra}
        <ul data-role="listview" data-inset="true">
          <li .ui-field-contain>
            <label>^{fvLabel nameView}
            ^{fvInput nameView}
          <li .ui-field-contain>
            <input type=submit value="Lisää rata">
      |]
  return (courseRes, widget)

newLayoutForm :: CourseId -> Html -> MForm Handler (FormResult (Layout, Int), Widget)
newLayoutForm cid extra = do
  (nameRes, nameView) <- mreq textField
    (FieldSettings "Layoutin nimi" Nothing Nothing Nothing
      [("placeholder","Layoutin nimi")]) Nothing
  (descRes, descView) <- mreq textField
    (FieldSettings "Layoutin kuvaus" Nothing Nothing Nothing
      [("placeholder","Layoutin kuvaus")]) Nothing
  (holesRes, holesView) <- mreq rangeField
    (FieldSettings "Väylien määrä" Nothing Nothing Nothing
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
            <input type=submit value="Lisää layout">
      |]
  return (res, widget)

newLayoutForm2 :: Html -> MForm Handler (FormResult (Layout, Int), Widget)
newLayoutForm2 extra = do
  (courseRes, courseView) <- mreq (selectField courses)
    "Rata" Nothing
  (nameRes, nameView) <- mreq textField
    (FieldSettings "Layoutin nimi" Nothing Nothing Nothing
      [("placeholder","Layoutin nimi")]) Nothing
  (descRes, descView) <- mreq textField
    (FieldSettings "Layoutin kuvaus" Nothing Nothing Nothing
      [("placeholder","Layoutin kuvaus")]) Nothing
  (holesRes, holesView) <- mreq rangeField
    (FieldSettings "Väylien määrä" Nothing Nothing Nothing
      [("min","1"),("max", "50")]) (Just 9)
  let layoutRes = Layout
                    <$> courseRes
                    <*> nameRes
                    <*> descRes
  let res = (,) <$> layoutRes <*> holesRes
  let widget = [whamlet|
        #{extra}
        <ul data-role="listview" data-inset="true">
          <li .ui-field-contain>
            <label>^{fvLabel courseView}
            ^{fvInput courseView}
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
            <input type=submit value="Lisää layout">
      |]
  return (res, widget)
  where
    courses :: Handler (OptionList CourseId)
    courses = do
      entities <- runDB $ selectList [] [Asc CourseName]
      optionsPairs $ for entities $
        \(Entity cid course) -> (courseName course, cid)

-- TODO
holesForm :: LayoutId -> Html -> MForm Handler (FormResult (LayoutId, [Hole]), Widget)
holesForm lid extra = do
  let set = (FieldSettings "Väylien ihannetulokset" Nothing Nothing Nothing [("data-icon","false")])
  holeFields <- forM [1..9] $ \h -> mreq (selectFieldList holes) set Nothing
  let (holeResults, holeViews) = unzip holeFields
  let widget = [whamlet|
        #{extra}
        <ul data-role="listview" data-inset="true">
          <li .ui-field-contain>
            <label>Väylien ihannetulokset
            <fieldset data-role="controlgroup" data-type="horizontal">
              $forall holeView <- holeViews
                ^{fvInput holeView}
          <li .ui-field-contain>
            <input type=submit value="Muokkaa layouttia">
      |]
  return (FormFailure [], widget)
  where
    holes :: [(Text, String)]
    holes = [("2", "2"),("3", "3"),("4", "4"),("5", "5"),("6", "6")]
    -- get holes from db
    holes2 :: Handler [OptionList HoleId]
    holes2 = do
      entities <- runDB $ selectList [HoleLayoutId ==. lid] [Asc HoleNumber]
      forM entities $ \(Entity hid _) -> optionsPairs $ for [2..6] $ \n -> (pack (show n), hid)
    holeCount :: Handler Int
    holeCount = runDB $ count [HoleLayoutId ==. lid]

startCompetitionForm :: CompetitionId -> Html
  -> MForm Handler (FormResult CompetitionId, Widget)
startCompetitionForm cid extra = do
  let widget = [whamlet|
        #{extra}
        <input type=submit value="Aloita kilpailu">
      |]
  return (pure cid, widget)

nextRoundForm :: CompetitionId -> Html
  -> MForm Handler (FormResult CompetitionId, Widget)
nextRoundForm cid extra = do
  let widget = [whamlet|
        #{extra}
        <input type=submit value="Seuraava kierros">
      |]
  return (pure cid, widget)

finishCompetitionForm :: CompetitionId -> Html
  -> MForm Handler (FormResult CompetitionId, Widget)
finishCompetitionForm cid extra = do
  let widget = [whamlet|
        #{extra}
        <input type=submit value="Lopeta kilpailu">
      |]
  return (pure cid, widget)

signUpForm :: CompetitionId -> Html
  -> MForm Handler (FormResult (Text, Text, D.Division), Widget)
signUpForm cid extra = do
  (nameRes, nameView) <- mreq textField
    (FieldSettings "Nimi" Nothing Nothing Nothing
      [("placeholder","Nimi")]) Nothing
  (emailRes, emailView) <- mreq emailField
    (FieldSettings "Sähköposti" Nothing Nothing Nothing
      [("placeholder","Sähköposti")]) Nothing
  (divisionRes, divisionView) <- mreq (radioFieldList D.divisions)
    "Luokka" Nothing
  let result = (,,) <$> nameRes <*> emailRes <*> divisionRes
  let widget = [whamlet|
        #{extra}
        <label>^{fvLabel nameView}
        ^{fvInput nameView}
        <label>^{fvLabel emailView}
        ^{fvInput emailView}
        <label>^{fvLabel divisionView}
        ^{fvInput divisionView}
        <input type=submit value="Ilmoittaudu">
      |]
  return (result, widget)

signUpFormLoggedIn :: CompetitionId -> Player -> Html
  -> MForm Handler (FormResult (Text, Text, D.Division), Widget)
signUpFormLoggedIn cid player extra = do
  (divisionRes, divisionView) <- mreq (radioFieldList D.divisions)
    "Luokka" Nothing
  let name = playerName player
      email = playerEmail player
  let result = (,,) <$> pure name <*> pure email <*> divisionRes
  let widget = [whamlet|
        #{extra}
        <label>^{fvLabel divisionView}
        ^{fvInput divisionView}
        <input type=submit value="Ilmoittaudu">
      |]
  return (result, widget)

scoreForm :: HoleId -> RoundId -> Text
   -> Html -> MForm Handler (FormResult Score, Widget)
scoreForm hid rid name extra = do
  r <- getUrlRender
  let set = (FieldSettings "" Nothing Nothing Nothing
        [ ("data-icon","false")
        , ("data-mini","true")
        , ("data-inline","true")
        , ("data-url", r (ScoreR rid hid))
        ])
  (scoreRes, scoreView) <- mreq (selectFieldList holes) set Nothing
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
    holes :: [(Text, Int)]
    holes = ("#", 0) : [(pack (show i), i) | i <- [1..99]]

profileForm :: Player -> Html -> MForm Handler (FormResult (Text, Text), Widget)
profileForm player extra = do
  (nameRes, nameView) <- mreq textField
    (FieldSettings "Nimi" Nothing Nothing Nothing
      [("placeholder","Nimi")]) (Just $ playerName player)
  (emailRes, emailView) <- mreq textField
    (FieldSettings "Sähköposti" Nothing Nothing Nothing
      [("placeholder","Sähköposti")]) (Just $ playerEmail player)
  let result =  (,) <$> nameRes <*> emailRes
  let widget = [whamlet|
        #{extra}
        <label>^{fvLabel nameView}
        ^{fvInput nameView}
        <label>^{fvLabel emailView}
        ^{fvInput emailView}
        <input type=submit value="Päivitä">
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