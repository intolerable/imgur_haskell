{-# OPTIONS -Wall #-}

module Main where

  import System.Environment ( getArgs )
  import qualified Imgur

  clientID :: Imgur.ClientID
  clientID = "7adfe26ebe75ec3"

  main :: IO ()
  main = getArgs >>= (Imgur.uploadFile clientID . head) >>= print