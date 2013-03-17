{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Blaze.Html5.Extra where

import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- | A POST form.
postForm :: Html -> Html
postForm = H.form ! A.method "POST"
