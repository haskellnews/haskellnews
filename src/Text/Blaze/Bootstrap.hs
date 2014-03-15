{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Blaze.Bootstrap where

import Text.Blaze.Html5
import Text.Blaze.Extra
import Prelude (($))

span1 :: Html -> Html
span1 x = div !. "span1 col-md-1" $ x

span2 :: Html -> Html
span2 x = div !. "span2 col-md-2" $ x

span3 :: Html -> Html
span3 x = div !. "span3 col-md-3" $ x

span4 :: Html -> Html
span4 x = div !. "span4 col-md-4" $ x

span5 :: Html -> Html
span5 x = div !. "span5 col-md-5" $ x

span6 :: Html -> Html
span6 x = div !. "span6 col-md-6" $ x

span7 :: Html -> Html
span7 x = div !. "span7 col-md-7" $ x

span8 :: Html -> Html
span8 x = div !. "span8 col-md-8" $ x

span9 :: Html -> Html
span9 x = div !. "span9 col-md-9" $ x

span10 :: Html -> Html
span10 x = div !. "span10 col-md-10" $ x

span11 :: Html -> Html
span11 x = div !. "span11 col-md-11" $ x

span12 :: Html -> Html
span12 x = div !. "span12 col-md-12" $ x

container :: Html -> Html
container x = div !. "container" $ x

row :: Html -> Html
row x = div !. "row" $ x

containerFluid :: Html -> Html
containerFluid x = div !. "container-fluid" $ x

inputAppend :: Html -> Html
inputAppend x = div !. "input-append" $ x
