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

main :: IO ()
main = do
    (selector:_) <- getArgs
    interact (f selector)

    where

    f selector    = pack
                >>> parseLT
                >>> fromDocument
                >>> stuffToError (toAxis . either error id . parsePath $ selector)
                >>> stuffToError child
                >>> stuffToError content
                >>> unpack

    stuffToEither f x = case listToMaybe (f x) of
                            Nothing -> Left (show x)
                            Just y -> Right y

    stuffToError :: Show a => (a -> [b]) -> a -> b
    stuffToError f x = (either error id) . (stuffToEither f) $ x
