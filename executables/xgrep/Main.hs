module Main where

import Control.Arrow ((>>>))
import Control.Monad
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Text as Strict (pack, unpack)
import Data.Text.Lazy qualified as Lazy (pack, toStrict)
import System.Environment (getArgs)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.HTML.DOM (parseLT)
import Text.XML (Node (NodeContent, NodeElement), def, renderText)
import Text.XML.Cursor (child, content, cut, fromDocument, node, (&/))
import Text.XML.Selectors.CSS (parsePath, toAxis)

main ∷ IO ()
main = do
  args ← getArgs
  let options = [("raw", "--raw" `elem` args)]
  let selector = head . dropWhile (isPrefixOf "--") $ args
  let axis = parsePath >>> either error id >>> toAxis $ selector
  interact' (f options axis)
 where
  interact' f = interact $ Lazy.pack >>> f >>> fmap unpack >>> unlines

  f options axis = parseLT >>> fromDocument >>> axis >=> g options

  g options cursor
    | (Just True) ← "raw" `lookup` options =
        pure . Lazy.toStrict . renderMarkup . toMarkup . node $ cursor
    | [NodeContent x] ← node <$> child cursor = pure x
    | (NodeElement x) ← node cursor = pure . pack . show $ x
    | otherwise = pure . (pack . show) . node $ cursor
