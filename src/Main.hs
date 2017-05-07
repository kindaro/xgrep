{-# LANGUAGE
    LambdaCase
  #-}

module Main where

import XML.Selectors.CSS (toAxis, parsePath)
import Text.HTML.DOM (parseLT)
import Text.XML (renderText, def)
import Text.XML.Cursor (fromDocument, cut, content, child)
import Control.Arrow ((>>>))
import Data.Text.Lazy (pack)
import Data.Text (unpack)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Control.Monad

main :: IO ()
main = do

    (selector:_) <- getArgs
    let axis = parsePath >>> either error id >>> toAxis $ selector
    interact' (f axis)

    where
    
    interact' f = interact $ pack >>> f >>> fmap unpack >>> unlines

    f axis = (parseLT >>> fromDocument >>> axis) >=> child >=> content

