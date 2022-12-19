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
import           PlutusTx.Prelude                   hiding ((<$>), mapM)
import           Prelude                            ((^), (<$>))
import qualified Prelude                            as Haskell
import           Test.QuickCheck                    (Arbitrary(..))

import           ENCOINS.BaseTypes                  (GroupElement, FieldElement, MintingPolarity)
import           ENCOINS.Crypto.Field
import           PlutusTx.Extra.ByteString          (ToBuiltinByteString (..))

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

------------------------------------------ Input --------------------------------------------

data Input = Input
    {
        inputCommit   :: GroupElement,
        inputPolarity :: MintingPolarity
    }
    deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

instance Eq Input where
    (==) (Input c p) (Input c' p') = c == c' && p == p'

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