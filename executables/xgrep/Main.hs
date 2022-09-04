module Main where

import Prelude.Unicode
import Prelude hiding (any)

import Control.Arrow ((>>>))
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as ByteStream
import Data.Char qualified as Char
import Data.Containers.ListUtils
import Data.Csv qualified as Csv
import Data.Distributive
import Data.Foldable qualified as Foldable
import Data.Function
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Maybe
import Data.Text qualified as Strict
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.IO qualified as Lazy
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word
import Options.Applicative hiding (columns)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Pretty (renderMarkup)
import Text.HTML.DOM (parseLBS, parseLT)
import Text.XML.Cursor (Axis, Cursor)
import Text.XML.Cursor qualified as XML

import Text.XML.Selectors.CSS
import Text.XML.Selectors.CSS.Parse

newtype XgrepException = XgrepException String deriving (Show)
instance Exception XgrepException

newtype Identifier = Identifier {path ∷ [Int]} deriving (Show, Eq, Ord)

countPrecedingSiblings ∷ Cursor → Int
countPrecedingSiblings = length ∘ XML.precedingSibling

identify ∷ Cursor → Identifier
identify =
  Identifier ∘ List.reverse ∘ fix
    \recurse cursor → countPrecedingSiblings cursor : maybe [] recurse (listToMaybe (XML.parent cursor))

joinCursors ∷ [[Cursor]] → [Cursor]
joinCursors = nubOrdOn identify ∘ List.sortOn identify ∘ concat

any ∷ Foldable foldable ⇒ foldable Axis → Axis
any = fmap joinCursors ∘ distribute ∘ Foldable.toList

data Options = Options
  { selectors ∷ Vector Strict.Text
  , columns ∷ Bool
  , numberOfColumns ∷ Maybe Int
  , delimiter ∷ Maybe Word8
  }

parseOptions ∷ Parser Options
parseOptions = do
  selectors ← (fmap Vector.fromList ∘ some ∘ strArgument) (metavar "selectors")
  columns ← switch (long "columns" <> help "Attempt to arrange the output in columns.")
  numberOfColumns ← optional
    do option auto (long "number-of-columns" <> help "Specify number of columns." <> metavar "{number}")
  delimiter ← optional
    do
      option
        do maybeReader ((cruelTrim ∘ Char.ord) <=< uncompromisingListToMaybe)
        do long "delimiter" <> help "Specify delimiter." <> metavar "{one byte character}"
  pure do Options{..}

uncompromisingListToMaybe ∷ [α] → Maybe α
uncompromisingListToMaybe [x] = Just x
uncompromisingListToMaybe _ = Nothing

cruelTrim ∷
  ∀ source target.
  (Integral source, Num source, Integral target, Bounded target, Num target, Ord target) ⇒
  source →
  Maybe target
cruelTrim source =
  if fromIntegral (minBound @target) ≤ source ∧ source ≤ fromIntegral (maxBound @target)
    then Just do fromIntegral source
    else Nothing

main ∷ IO ()
main = do
  options@Options{..} ← execParser do
    info
      do helper <*> parseOptions
      do fullDesc <> progDesc "Cut through your XML with CSS selectors!"
  axes ← traverse (parsePath >>> either (throwIO ∘ XgrepException) pure >>> fmap toAxis) selectors
  if columns
    then
      let interaction =
            parseLBS
              >>> XML.fromDocument
              >>> any axes
              >>> fmap (textifyCursor options)
              >>> Split.chunksOf (fromMaybe (length selectors) numberOfColumns)
              >>> Csv.encodeWith do
                Csv.defaultEncodeOptions{Csv.encDelimiter = fromMaybe ((fromIntegral ∘ Char.ord) '\t') delimiter}
       in ByteStream.interact interaction
    else Lazy.interact (match options (any axes) >>> Lazy.fromChunks)

match ∷ Options → Axis → Lazy.Text → [Strict.Text]
match options axis = parseLT >>> XML.fromDocument >>> axis >>> fmap (textifyCursor options)

textifyCursor ∷ Options → Cursor → Strict.Text
textifyCursor Options{columns = True} =
  Strict.unwords ∘ filter (not ∘ Strict.null) ∘ fmap Strict.strip ∘ (XML.content <=< contentNodes)
textifyCursor Options{columns = False} = Strict.pack ∘ renderMarkup ∘ toMarkup ∘ XML.node
