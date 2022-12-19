{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Bulletproofs.Types where

import           Control.Monad.Extra                (mapM)
import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)
import           PlutusTx                           (FromData(..), UnsafeFromData(..), ToData(..))
import           PlutusTx.Prelude                   hiding ((<$>), mapM)
import           Prelude                            ((^), (<$>))
import qualified Prelude                            as Haskell
import           Test.QuickCheck                    (Arbitrary(..))

import           ENCOINS.BaseTypes                  (GroupElement, FieldElement, MintingPolarity)
import           ENCOINS.Crypto.Field
import           PlutusTx.Extra.ByteString          (ToBuiltinByteString (..))
import           PlutusTx.Extra.Prelude             (drop)

------------------------------------- BulletproofSetup --------------------------------------

bulletproofN :: Integer
bulletproofN = 10

bulletproofM :: Integer
bulletproofM = 10

data BulletproofSetup = BulletproofSetup GroupElement GroupElement [GroupElement] [GroupElement]
    deriving (Haskell.Eq, Haskell.Show)

instance Eq BulletproofSetup where
    (==) (BulletproofSetup h g hs gs) (BulletproofSetup h' g' hs' gs') =
        h == h' && g == g' && hs == hs' && gs == gs'

instance Arbitrary BulletproofSetup where
    arbitrary = do
        h  <- arbitrary
        g  <- arbitrary
        hs <- mapM (const arbitrary) [1..(bulletproofN * bulletproofM)]
        gs <- mapM (const arbitrary) [1..(bulletproofN * bulletproofM)]
        return $ BulletproofSetup h g hs gs

instance FromData BulletproofSetup where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData dat = do
        lst  <- fromBuiltinData dat
        let n    = bulletproofN * bulletproofM
            lst' = drop 2 lst
        if length lst /= 2*n + 2
            then Nothing
            else Just $ BulletproofSetup (lst !! 0) (lst !! 1) (take n lst') (drop n lst')

instance UnsafeFromData BulletproofSetup where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData dat  = BulletproofSetup (lst !! 0) (lst !! 1) (take n lst') (drop n lst')
        where
            lst  = unsafeFromBuiltinData dat
            lst' = drop 2 lst
            n    = bulletproofN * bulletproofM

instance ToData BulletproofSetup where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (BulletproofSetup h g hs gs) = toBuiltinData $ [h, g] ++ hs ++ gs

------------------------------------ BulletproofParams --------------------------------------

-- A type that encodes public input parameters: deposit/withdrawal address public key and validity interval
type BulletproofParams = GroupElement

------------------------------------------ Secret -------------------------------------------

data Secret = Secret
    {
        secretGamma :: FieldElement,
        secretV     :: FieldElement
    }
    deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

-- unstableMakeIsData ''Secret

instance Eq Secret where
    (==) (Secret g1 v1) (Secret g2 v2) = g1 == g2 && v1 == v2

instance Arbitrary Secret where
    arbitrary = do
        gamma <- arbitrary
        v     <- F . (`modulo` (2 ^ bulletproofN)) <$> arbitrary
        return $ Secret gamma v

instance FromData Secret where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData dat = do
        (g, v)  <- fromBuiltinData dat
        Just $ Secret g v

instance UnsafeFromData Secret where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData dat  = Secret g v
        where (g, v)  = unsafeFromBuiltinData dat

instance ToData Secret where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (Secret g v) = toBuiltinData (g, v)

type Secrets = [Secret]

---------------------------------------- Randomness -----------------------------------------

data Randomness = Randomness FieldElement [FieldElement] [FieldElement] FieldElement FieldElement FieldElement
    deriving (Haskell.Eq, Haskell.Show, Generic)

instance Eq Randomness where
    (==) (Randomness alpha sL sR rho tau1 tau2) (Randomness alpha' sL' sR' rho' tau1' tau2') =
        alpha == alpha' && sL == sL' && sR == sR' && rho == rho' && tau1 == tau1' && tau2 == tau2'

instance Arbitrary Randomness where
    arbitrary = do
        alpha <- arbitrary
        sL    <- mapM (const arbitrary) [1..(bulletproofN * bulletproofM)]
        sR    <- mapM (const arbitrary) [1..(bulletproofN * bulletproofM)]
        rho   <- arbitrary
        tau1  <- arbitrary
        Randomness alpha sL sR rho tau1 <$> arbitrary

instance FromData Randomness where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData dat = do
        lst  <- fromBuiltinData dat
        let n    = bulletproofN * bulletproofM
            lst' = drop 4 lst
        if length lst /= 2*n + 4
            then Nothing
            else Just $ Randomness (lst !! 0) (take n lst') (drop n lst') (lst !! 1) (lst !! 2) (lst !! 3)

instance UnsafeFromData Randomness where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData dat  = Randomness (lst !! 0) (take n lst') (drop n lst') (lst !! 1) (lst !! 2) (lst !! 3)
        where
            lst  = unsafeFromBuiltinData dat
            lst' = drop 4 lst
            n    = bulletproofN * bulletproofM

instance ToData Randomness where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (Randomness alpha sL sR rho tau1 tau2) = toBuiltinData $ [alpha, rho, tau1, tau2] ++ sL ++ sR

------------------------------------------ Input --------------------------------------------

data Input = Input
    {
        inputCommit   :: GroupElement,
        inputPolarity :: MintingPolarity
    }
    deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

instance Eq Input where
    (==) (Input c p) (Input c' p') = c == c' && p == p'

instance FromData Input where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData dat = do
        (g, p)  <- fromBuiltinData dat
        Just $ Input g p

instance UnsafeFromData Input where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData dat  = Input g p
        where (g, p)  = unsafeFromBuiltinData dat

instance ToData Input where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (Input g p) = toBuiltinData (g, p)

type Inputs = [Input]

------------------------------------------ Proof --------------------------------------------

data Proof = Proof GroupElement GroupElement GroupElement GroupElement FieldElement FieldElement [FieldElement] [FieldElement] FieldElement
    deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

instance ToBuiltinByteString Proof where
    {-# INLINABLE toBytes #-}
    toBytes (Proof commitA commitS commitT1 commitT2 taux mu lx rx tHat) =
        toBytes [commitA, commitS, commitT1, commitT2] `appendByteString`
        toBytes ([taux, mu, tHat] ++ lx ++ rx)

instance Eq Proof where
    (==) (Proof commitA commitS commitT1 commitT2 taux mu lx rx tHat) (Proof commitA' commitS' commitT1' commitT2' taux' mu' lx' rx' tHat') =
        commitA == commitA' && commitS == commitS' && commitT1 == commitT1' && commitT2 == commitT2' &&
        taux == taux' && mu == mu' && lx == lx' && rx == rx' && tHat == tHat'

instance FromData Proof where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData dat = do
        (lstG, lstF)  <- fromBuiltinData dat
        let n    = bulletproofN * bulletproofM
            lst' = drop 3 lstF
        if length lstF /= 2*n + 3 && length lstG /= 4
            then Nothing
            else Just $ Proof (lstG !! 0) (lstG !! 1) (lstG !! 2) (lstG !! 3)
                    (lstF !! 0) (lstF !! 1) (take n lst') (drop n lst') (lstF !! 2)

instance UnsafeFromData Proof where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData dat  = Proof (lstG !! 0) (lstG !! 1) (lstG !! 2) (lstG !! 3)
            (lstF !! 0) (lstF !! 1) (take n lst') (drop n lst') (lstF !! 2)
        where
            (lstG, lstF) = unsafeFromBuiltinData dat
            lst' = drop 3 lstF
            n    = bulletproofN * bulletproofM

instance ToData Proof where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (Proof commitA commitS commitT1 commitT2 taux mu lx rx tHat) =
        toBuiltinData ((commitA, commitS, commitT1, commitT2), [taux, mu, tHat] ++ lx ++ rx)