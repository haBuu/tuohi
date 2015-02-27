module Datepicker where

import Import

addDatepicker = do
  addScript $ StaticR js_bootstrap_datepicker_js
  -- add more languages here (currently finnish and english)
  addScript $ StaticR js_locales_bootstrap_datepicker_fi_js
  addStylesheet $ StaticR css_datepicker3_css