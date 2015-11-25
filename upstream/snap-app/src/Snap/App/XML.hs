module Snap.App.XML where

import Text.XML.Light
import Snap.App
import qualified Data.Text as T

-- | Output the given XML element.
outputXML :: Element -> Controller c s ()
outputXML = writeText . T.pack . showElement
