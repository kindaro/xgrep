{-# LANGUAGE
    PatternGuards
  #-}

module Main where

import XML.Selectors.CSS (toAxis, parsePath)
import Text.HTML.DOM (parseLT)
import Text.XML (renderText, def, Node(NodeElement, NodeContent))
import Text.XML.Cursor (fromDocument, cut, content, child, node)
import Control.Arrow ((>>>))
import qualified Data.Text.Lazy as Lazy (pack)
import Data.Text as Strict (pack, unpack)

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Control.Monad

main :: IO ()
main = do

    (selector:_) <- getArgs
    let axis = parsePath >>> either error id >>> toAxis $ selector
    interact' (f axis)

    where
    
    interact' f = interact $ Lazy.pack >>> f >>> fmap unpack >>> unlines

    f axis = (parseLT >>> fromDocument >>> axis) >=> child >=> g

    g cursor
        | (NodeElement x) <- node cursor = pure . (pack . show) $ x
        | (NodeContent x) <- node cursor = pure x
        | otherwise = pure . (pack . show) . node $ cursor

