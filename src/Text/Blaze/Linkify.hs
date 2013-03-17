-- | Render some text to HTML, replacing any URIs with actual links.

module Text.Blaze.Linkify where

import Data.Text (Text)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Links

-- | Render some text to HTML, replacing any URIs with actual links.
linkify :: Text -> Html
linkify = mapM_ renderOrLeave . explodeLinks where
  renderOrLeave (Right text) = toHtml text
  renderOrLeave (Left uri) = a ! href (toValue (show uri)) $ toHtml (show uri)
