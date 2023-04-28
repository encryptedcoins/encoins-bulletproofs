{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Bulletproofs.Common where

import           PlutusTx.Prelude
import           Prelude                          (Show)

import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs.Types       (BulletproofSetup (..), bulletproofN)
import           ENCOINS.Bulletproofs.Utils
import           ENCOINS.Crypto.Field

data CommonPart = CommonPart FieldElement FieldElement [FieldElement] [FieldElement] [FieldElement] [GroupElement]
    deriving Show

commonPart :: BulletproofSetup -> [GroupElement] -> [MintingPolarity] -> (GroupElement, GroupElement) -> CommonPart
commonPart (BulletproofSetup _ _ hs _) gs ps (commitA, commitS) = CommonPart z z' ys zs lam hs'
    where
        m        = length ps
        twos     = powers (F 2) bulletproofN
        (y, z)   = challenge $ [commitA, commitS] ++ gs
        ys       = powers y (bulletproofN * m)
        (zs, z') = powersOfZ z m
        lam1     = map (* z) ys
        lam2     = concat $ zipWith (\pj zj -> map (\a -> (a * zj) + (withPolarity pj a *  z')) twos) ps zs
        lam      = zipWith (+) lam1 lam2
        hs'      = zipWith groupExp hs (map inv ys)