-- | Splices

module ParserExtra (customOptions) where

import           Data.Aeson.TH (Options (..), defaultOptions)

customOptions = defaultOptions { fieldLabelModifier = tail . dropWhile (/= '_') }
