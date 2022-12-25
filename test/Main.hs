{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Data.Maybe                       (fromJust)
import           Prelude                          (IO, print, unzip)
import qualified Prelude                          as Haskell
import           System.Random                    (randomRIO, mkStdGen, Random (..))
import           Test.QuickCheck                  (quickCheck)

import           Tests.Verification               (prop_verification)

main :: IO ()
main = do
    quickCheck prop_verification
