{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Error where

import Import

emailExists :: SomeException -> Handler ()
emailExists e = do
  setMessageI MsgEmailExists