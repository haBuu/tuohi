{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.I18n.Finnish where

import Yesod.Form.Types (FormMessage (..))
import Data.Monoid (mappend)
import Data.Text (Text)

finnishMessage :: FormMessage -> Text
finnishMessage (MsgInvalidInteger t) = "Virheellinen kokonaisluku: " `mappend` t
finnishMessage (MsgInvalidNumber t) = "Virheellinen numero: " `mappend` t
finnishMessage (MsgInvalidEntry t) = "Invalid entry: " `mappend` t
finnishMessage MsgInvalidTimeFormat = "Virheellinen aika, täytyy olla muodossa TT:MM[:SS]"
finnishMessage MsgInvalidDay = "Virheellinen päivämäärä, täytyy olla muodossa VVVV-KK-PP"
finnishMessage (MsgInvalidUrl t) = "Virheellinen URL: " `mappend` t
finnishMessage (MsgInvalidEmail t) = "Virheellinen sähköpostiosoite: " `mappend` t
finnishMessage (MsgInvalidHour t) = "Virheellinen tunti: " `mappend` t
finnishMessage (MsgInvalidMinute t) = "Virheellinen minuutti: " `mappend` t
finnishMessage (MsgInvalidSecond t) = "Virheellinen sekunti: " `mappend` t
finnishMessage MsgCsrfWarning = "Suojaksi cross-site request forgery hyökkäyksiä vastaan, olkaa hyvä ja vahvistakaa lomakkeen lähetys."
finnishMessage MsgValueRequired = "Arvo on pakollinen"
finnishMessage (MsgInputNotFound t) = "Syötettä ei löytynyt: " `mappend` t
finnishMessage MsgSelectNone = "<Tyhjä>"
finnishMessage (MsgInvalidBool t) = "Virheellinen totuusarvo: " `mappend` t
finnishMessage MsgBoolYes = "Kyllä"
finnishMessage MsgBoolNo = "Ei"
finnishMessage MsgDelete = "Poista?"