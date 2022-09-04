module Main where

import Prelude.Unicode
import Prelude hiding (any)

import Control.Arrow ((>>>))
import Control.Exception
import Control.Monad
import Data.Containers.ListUtils
import Data.Foldable qualified as Foldable
import Data.Function
import Data.List qualified as List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Strict
import Data.Text.Lazy qualified as Lazy (pack, toStrict)
import Data.Traversable
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Options.Applicative
import System.Environment (getArgs)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.HTML.DOM (parseLT)
import Text.XML (Node (NodeContent, NodeElement), def, renderText)
import Text.XML.Cursor
import Data.Distributive
import Text.Show.Pretty

import Text.XML.Selectors.CSS (parsePath, toAxis)

newtype XgrepException = XgrepException String deriving (Show)
instance Exception XgrepException

newtype Identifier = Identifier {path ∷ [Int]} deriving (Show, Eq, Ord)

countPrecedingSiblings ∷ Cursor → Int
countPrecedingSiblings = length ∘ precedingSibling

identify ∷ Cursor → Identifier
identify =
  Identifier ∘ List.reverse ∘ fix
    \recurse cursor → countPrecedingSiblings cursor : maybe [] recurse (listToMaybe (parent cursor))

joinCursors ∷ [[Cursor]] → [Cursor]
joinCursors = nubOrdOn identify ∘ List.sortOn identify ∘ concat

any ∷ Foldable foldable ⇒ foldable Axis → Axis
any = fmap joinCursors ∘ distribute ∘ Foldable.toList

newtype Options = Options
  { selectors ∷ Vector Strict.Text
  }

parseOptions ∷ Parser Options
parseOptions = do
  selectors ← (fmap Vector.fromList ∘ some ∘ strArgument) (metavar "selector")
  pure do Options{..}

main ∷ IO ()
main = do
  Options{..} ← execParser do
    info
      do helper <*> parseOptions
      do fullDesc <> progDesc "Cut through your XML with CSS selectors!"
  axes ← traverse (parsePath >>> either (throwIO ∘ XgrepException) pure >>> fmap toAxis) selectors
  interact' ((f ∘ any) axes)
 where
  interact' f = interact $ Lazy.pack >>> f >>> fmap Strict.unpack >>> unlines

  f axis = parseLT >>> fromDocument >>> axis >=> g

  g ∷ Cursor → [Strict.Text]
  g cursor
    | (NodeContent x) ← node cursor = pure x
    | [NodeContent x] ← node <$> child cursor = pure x
    | (NodeElement x) ← node cursor = pure . Strict.pack . ppShow $ x
    | otherwise = pure . (Strict.pack . ppShow) . node $ cursor
