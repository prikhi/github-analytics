{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ do
    el "h1" $ text "Hello World!"
    el "p" $ text "Body Text!"
