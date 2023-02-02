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

import           Prelude                                                  
import           Test.QuickCheck                  (quickCheck)

import           Tests.Group                      (prop_group_associativity, prop_group_identity, prop_group_inverse)
import           Tests.Verification               (prop_verification)

main :: IO ()
main = do
    quickCheck prop_group_associativity
    quickCheck prop_group_identity
    quickCheck prop_group_inverse
    quickCheck prop_verification