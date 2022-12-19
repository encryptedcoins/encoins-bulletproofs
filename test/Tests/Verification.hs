{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Tests.Verification where

import           Control.Monad                    (mapM)
import           Data.Maybe                       (fromJust)
import           PlutusTx.Prelude                 hiding ((<$>), mapM)
import           Prelude                          (IO, print, unzip, (<$>), Show (..))
import qualified Prelude                          as Haskell
import           Test.QuickCheck                  (quickCheck, Arbitrary (..))

import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs

data TestVerification = TestVerification BulletproofSetup BulletproofParams Secrets [MintingPolarity] Randomness
    deriving (Haskell.Eq)

instance Haskell.Show TestVerification where
    show (TestVerification bs bp secrets mps r) =
        "BulletproofParams: " ++ show bp ++ "\n\n" ++
        "Secrets: " ++ show secrets ++ "\n\n" ++
        "MintingPolarities: " ++ show mps ++ "\n\n" ++
        "Randomness: " ++ show r

instance Arbitrary TestVerification where
    arbitrary = do
        m <- (+1) . (`modulo` 10) <$> arbitrary
        bs <- arbitrary
        bp <- arbitrary
        secrets <- mapM (const arbitrary) [1..m]
        mps     <- mapM (const arbitrary) [1..m]
        TestVerification bs bp secrets mps <$> arbitrary

prop_verification :: TestVerification -> Bool
prop_verification (TestVerification bs bp secrets mps r) = verify bs bp val ins proof
    where (val, ins, proof) = bulletproof bs bp secrets mps r