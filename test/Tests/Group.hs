{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Tests.Group where

import           Prelude                          
import           Test.QuickCheck                  (quickCheck, Arbitrary (..))

import           ENCOINS.BaseTypes                (GroupElement, groupIdentity, groupMul, groupExp)
import           ENCOINS.Crypto.Field             (toFieldElement)

type TestGroup = (GroupElement, GroupElement, GroupElement)

prop_group_associativity :: TestGroup -> Bool
prop_group_associativity (a, b, c) = (a `groupMul` b) `groupMul` c == a `groupMul` (b `groupMul` c)

prop_group_identity :: TestGroup -> Bool
prop_group_identity (a, b, c) = (groupIdentity `groupMul` a == a) && (a `groupMul` groupIdentity == a)

prop_group_inverse :: TestGroup -> Bool
prop_group_inverse (a, b, c) = (a `groupMul` a_inv == groupIdentity) && (a_inv `groupMul` a == groupIdentity)
    where a_inv = groupExp a (toFieldElement (-1))