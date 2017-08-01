{-# LANGUAGE
    PatternGuards
  #-}

module Main where

import XML.Selectors.CSS (toAxis, parsePath)
import Text.HTML.DOM (parseLT)
import Text.XML (renderText, def, Node(NodeElement, NodeContent))
import Text.XML.Cursor (fromDocument, cut, content, child, node, (&/))
import Control.Arrow ((>>>))
import qualified Data.Text.Lazy as Lazy (pack, toStrict)
import Data.Text as Strict (pack, unpack)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.Text (renderMarkup)

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Control.Monad
import Data.List (isPrefixOf)

main :: IO ()
main = do

    args <- getArgs
    let options = [ ("raw", "--raw" `elem` args) ]
    let selector = head . dropWhile (isPrefixOf "--") $ args
    let axis = parsePath >>> either error id >>> toAxis $ selector
    interact' (f options axis)

    where
    
    interact' f = interact $ Lazy.pack >>> f >>> fmap unpack >>> unlines

    f options axis = parseLT >>> fromDocument >>> (axis &/ g options)

    g options cursor
        | (Just True) <- "raw" `lookup` options =
            pure . Lazy.toStrict . renderMarkup . toMarkup . node $ cursor
        | (NodeElement x) <- node cursor = pure . (pack . show) $ x
        | (NodeContent x) <- node cursor = pure x
        | otherwise = pure . (pack . show) . node $ cursor

