{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Bulletproofs.Verify where

import           PlutusTx.Prelude

import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs.Common
import           ENCOINS.Bulletproofs.Utils  (challenge, powers, polarityToInteger)
import           ENCOINS.Bulletproofs.Types
import           ENCOINS.Crypto.Field

{-# INLINABLE verify #-}
verify :: BulletproofSetup -> BulletproofParams -> Integer -> Inputs -> Proof -> Bool
verify bs@(BulletproofSetup h g _ gs) bp val inputs (Proof commitA commitS commitT1 commitT2 taux mu lx rx tHat) = cond0 && cond1 && cond2 && cond3
    where
        m        = length inputs
        ps       = map inputPolarity inputs
        commitVs = map inputCommit inputs

        gVal     = groupExp g $ toFieldElement val
        gInputs  = [bp, gVal] ++ commitVs

        CommonPart z z' ys zs lam hs' = commonPart bs gInputs ps (commitA, commitS)

        (x, _)   = challenge $ [commitA, commitS, commitT1, commitT2] ++ gInputs
        x2       = x * x

        commitP  = foldl groupMul groupIdentity
            (commitA : groupExp commitS x : map (`groupExp` negate z) (take (bulletproofN*m) gs) ++ zipWith groupExp hs' lam)
        twos     = powers (F 2) bulletproofN
        s        = sum twos
        psSum    = F $ sum $ map polarityToInteger ps
        delta    = ((z - z*z) * sum ys) - z * s * sum zs - z * s * z' * psSum + z' * toFieldElement val

        tHat' = bytes2fe tHat
        taux' = bytes2fe taux
        mu'   = bytes2fe mu
        lx'   = map bytes2fe lx
        rx'   = map bytes2fe rx

        cond0    = m <= bulletproofM && length lx' == bulletproofN*m && length rx' == bulletproofN*m
        cond1    = groupExp g tHat' `groupMul` groupExp h taux' ==
            groupExp g delta
            `groupMul` foldl groupMul groupIdentity (zipWith groupExp commitVs zs)
            `groupMul` groupExp commitT1 x
            `groupMul` groupExp commitT2 x2
        cond2    = commitP == foldl groupMul groupIdentity (groupExp h mu' : zipWith groupExp gs lx' ++ zipWith groupExp hs' rx')
        cond3    = tHat' == foldl (+) zero (zipWith (*) lx' rx')

