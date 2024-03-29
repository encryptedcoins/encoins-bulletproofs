{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Bulletproofs.Prove where

import           PlutusTx.Prelude

import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs.Common
import           ENCOINS.Bulletproofs.Types
import           ENCOINS.Bulletproofs.Utils
import           ENCOINS.Crypto.Field

-- NOTE: Here we assume that all lists have the correct lengths. Currently, the client should take care of it.
bulletproof :: BulletproofSetup -> BulletproofParams -> Secrets -> [MintingPolarity] -> Randomness -> (Integer, Inputs, Proof)
bulletproof bs@(BulletproofSetup h g hs gs) bp secrets ps (Randomness alpha sL sR rho tau1 tau2) = (val, zipWith Input commitVs ps,
        Proof commitA commitS commitT1 commitT2 (fe2bytes taux) (fe2bytes mu) (map fe2bytes lx) (map fe2bytes rx) (fe2bytes tHat))
    where
        m        = length secrets
        gammas   = map secretGamma secrets
        vs       = map secretV secrets
        val      = sum (zipWith (\v p -> fromFieldElement v * polarityToInteger p) vs ps)
        commitVs = zipWith groupMul (map (groupExp h) gammas) (map (groupExp g) vs)

        gVal     = groupExp g $ toFieldElement val
        gInputs  = [bp, gVal] ++ commitVs

        sL'      = take (bulletproofN * m) sL
        sR'      = take (bulletproofN * m) sR

        aL       = concatMap (fromBits . padBits bulletproofN . toBits) vs
        aR       = concatMap (fromBits . map (\q -> q - 1) . padBits bulletproofN . toBits) vs
        commitA  = foldl groupMul groupIdentity (groupExp h alpha : zipWith groupExp gs aL  ++ zipWith groupExp hs aR)
        commitS  = foldl groupMul groupIdentity (groupExp h rho   : zipWith groupExp gs sL' ++ zipWith groupExp hs sR')

        CommonPart z _ ys zs lam _ = commonPart bs gInputs ps (commitA, commitS)

        l        = (map (\a -> a - z) aL, sL')
        r        = (zipWith (+) (zipWith (*) ys aR) lam, zipWith (*) ys sR')
        (_, t1, t2) = polyProduct l r
        commitT1 = groupMul (groupExp g t1) (groupExp h tau1)
        commitT2 = groupMul (groupExp g t2) (groupExp h tau2)

        (x, _)   = challenge $ [commitA, commitS, commitT1, commitT2] ++ gInputs
        x2       = x * x

        taux     = (tau2 * x2) + (tau1 * x) + foldl (+) zero (zipWith (*) zs gammas)
        mu       = alpha + (rho * x)
        lx       = polyEvaluate l x
        rx       = polyEvaluate r x
        tHat     = foldl (+) zero $ zipWith (*) lx rx

fromSecret :: BulletproofSetup -> Secret -> (Integer, BuiltinByteString)
fromSecret (BulletproofSetup h g _ _) (Secret gamma v) = (val, bs)
    where
        val = fromFieldElement v
        bs  = fromGroupElement $ groupExp h gamma `groupMul` groupExp g v