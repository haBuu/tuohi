{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Error where

import Import

emailExists :: SomeException -> Handler ()
emailExists _ = do
  setMessageI MsgEmailExists