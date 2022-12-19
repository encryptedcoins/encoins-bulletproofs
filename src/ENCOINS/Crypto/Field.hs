{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Crypto.Field where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Bifunctor            (Bifunctor(..))
import           Data.Functor              ((<$>))
import           GHC.Generics              (Generic)
import           PlutusTx                  (ToData(..), FromData(..), UnsafeFromData(..))
import           PlutusTx.Prelude          hiding ((<$>))
import qualified Prelude                   as Haskell
import           System.Random             (Random (..), Uniform, UniformRange)
import           System.Random.Stateful    (Uniform(..), UniformRange (..))
import           Test.QuickCheck           (Arbitrary(..))

import           PlutusTx.Extra.ByteString

class (Monoid c) => FiniteField c where
    fieldPrime :: c -> Integer

newtype Field c = F Integer
    deriving (Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

{-# INLINABLE toFieldElement #-}
toFieldElement :: forall c . FiniteField c => Integer -> Field c
toFieldElement u = F $ modulo u (fieldPrime (mempty :: c))

{-# INLINABLE fromFieldElement #-}
fromFieldElement :: forall c . FiniteField c => Field c -> Integer
fromFieldElement (F u) = modulo u (fieldPrime (mempty :: c))

instance FiniteField c => Haskell.Eq (Field c) where
    (==) = (==)

instance FiniteField c => Eq (Field c) where
    {-# INLINABLE (==) #-}
    (==) (F u) (F v) = modulo (u - v) (fieldPrime (mempty :: c)) == 0

instance forall c. FiniteField c => Ord (Field c) where
    {-# INLINABLE (<=) #-}
    (<=) (F a) (F b) = modulo a (fieldPrime (mempty :: c)) <= modulo b (fieldPrime (mempty :: c))

instance forall c. FiniteField c => Haskell.Ord (Field c) where
    (<=) = (<=)

instance FiniteField c => AdditiveSemigroup (Field c) where
    {-# INLINABLE (+) #-}
    (+) (F u) (F v) = F $ modulo (u + v) (fieldPrime (mempty :: c))

instance FiniteField c => AdditiveGroup (Field c) where
    {-# INLINABLE (-) #-}
    (-) (F u) (F v) = F $ modulo (u - v) (fieldPrime (mempty :: c))

instance FiniteField c => AdditiveMonoid (Field c) where
    {-# INLINABLE zero #-}
    zero = F 0

instance FiniteField c => MultiplicativeSemigroup (Field c) where
    {-# INLINABLE (*) #-}
    (*) (F u) (F v) = F $ modulo (u * v) (fieldPrime (mempty :: c))

instance FiniteField c => MultiplicativeMonoid (Field c) where
    {-# INLINABLE one #-}
    one = F 1

instance FiniteField c => Semigroup (Field c) where
    {-# INLINABLE (<>) #-}
    (<>) = (*)

instance FiniteField c => Monoid (Field c) where
    {-# INLINABLE mempty #-}
    mempty = one

instance FiniteField c => Group (Field c) where
    {-# INLINABLE inv #-}
    inv (F u) = F (modulo (snd $ f (u, 1) (fieldPrime (mempty :: c), 0)) (fieldPrime (mempty :: c)))
      where
        f (x, y) (x', y')
          | x' == zero = (x, y)
          | otherwise  = f (x', y') (x - q * x', y - q * y')
          where q = divide x x'

instance ToBuiltinByteString (Field c) where
    {-# INLINABLE toBytes #-}
    toBytes (F u) = toBytes u

instance FiniteField c => Arbitrary (Field c) where
  {-# INLINABLE arbitrary #-}
  arbitrary = do
    n <- arbitrary
    return $ F $ modulo n (fieldPrime (mempty :: c))

instance UniformRange (Field c) where
    uniformRM (F u, F v) g = F <$> uniformRM (u, v) g

instance FiniteField c => Uniform (Field c) where
    uniformM = uniformRM (zero, F $ fieldPrime (mempty :: c) - 1)

instance FiniteField c => Random (Field c) where
    randomR (F u, F v) g = first F $ randomR (u, v) g
    randomRs (F u, F v)  = map F . randomRs (u, v)
    random               = randomR (zero, F $ fieldPrime (mempty :: c) - 1)
    randoms              = map F . randoms

instance FromData (Field c) where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData = fmap F . fromBuiltinData

instance UnsafeFromData (Field c) where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = F . unsafeFromBuiltinData

instance ToData (Field c) where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (F u) = toBuiltinData u