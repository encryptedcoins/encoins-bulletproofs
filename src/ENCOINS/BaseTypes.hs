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

module ENCOINS.BaseTypes where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Bool                 (bool)
import           Data.Functor              ((<$>))
import           GHC.Generics              (Generic)
import           PlutusTx                  (ToData(..), FromData(..), UnsafeFromData(..))
import           PlutusTx.Prelude          hiding ((<$>))
import qualified Prelude                   as Haskell
import           Test.QuickCheck           (Arbitrary(..))

import           ENCOINS.Crypto.Curve
import           ENCOINS.Crypto.Field
import           PlutusTx.Extra.ByteString

------------------------------------- Field Element --------------------------------------

type FieldElement = Field BLS12381Field

------------------------------------- Group Element --------------------------------------

newtype GroupElement = GroupElement (Point BLS12381)
    deriving (Haskell.Show, Generic, FromJSON, ToJSON)

instance Haskell.Eq GroupElement where
    (==) = (==)

instance Eq GroupElement where
    (==) (GroupElement (x1, y1)) (GroupElement (x2, y2)) = x1 == x2 && y1 == y2

instance ToBuiltinByteString GroupElement where
    {-# INLINABLE toBytes #-}
    toBytes (GroupElement (x, y)) = toBytes (x, y)

instance Arbitrary GroupElement where
    arbitrary = groupExp groupGenerator <$> arbitrary

instance FromData GroupElement where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData = fmap GroupElement . fromBuiltinData

instance UnsafeFromData GroupElement where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = GroupElement . unsafeFromBuiltinData

instance ToData GroupElement where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData (GroupElement g) = toBuiltinData g

{-# INLINABLE toGroupElement #-}
toGroupElement :: BuiltinByteString -> Maybe GroupElement
toGroupElement bs = do
        (_, y) <- fromX $ F x
        if x == fieldPrime BLS12381
            then Just $ GroupElement (F 0, F 0)
            else if toBytes (even $ fromFieldElement y) == takeByteString 1 bs
                then Just $ GroupElement (F x, y)
                else Just $ GroupElement (F x, negate y)
    where
          bs' = dropByteString 1 bs
          x = byteStringToInteger bs'

{-# INLINABLE fromGroupElement #-}
fromGroupElement :: GroupElement -> BuiltinByteString
fromGroupElement (GroupElement (x, y)) = toBytes isEven `appendByteString` toBytes n
    where (n, isEven) = if x == zero && y == zero
            then (fieldPrime BLS12381, False)
            else (fromFieldElement x, even $ fromFieldElement y)

{-# INLINABLE groupIdentity #-}
groupIdentity :: GroupElement
groupIdentity = GroupElement (F 0, F 0)

{-# INLINABLE groupGenerator #-}
groupGenerator :: GroupElement
groupGenerator = GroupElement gen

{-# INLINABLE groupMul #-}
groupMul :: GroupElement -> GroupElement -> GroupElement
groupMul (GroupElement (x1, y1)) (GroupElement (x2, y2)) = GroupElement $ addPointsJ (x1, y1) (x2, y2)

{-# INLINABLE groupExp #-}
groupExp :: GroupElement -> FieldElement -> GroupElement
groupExp (GroupElement (x, y)) n = GroupElement $ fromJ $ pointMulJ (toJ (x, y)) (fromFieldElement n)

------------------------------------- Minting Polarity --------------------------------------

data MintingPolarity = Mint | Burn
    deriving (Haskell.Eq, Haskell.Show, Generic, FromJSON, ToJSON)

instance Eq MintingPolarity where
    Mint == Mint = True
    Burn == Burn = True
    _ == _       = False

instance Arbitrary MintingPolarity where
    arbitrary = bool Mint Burn <$> arbitrary

instance ToBuiltinByteString MintingPolarity where
    {-# INLINABLE toBytes #-}
    toBytes = toBytes . (==) Mint

instance FromData MintingPolarity where
    {-# INLINABLE fromBuiltinData #-}
    fromBuiltinData = fmap (bool Mint Burn) . fromBuiltinData

instance UnsafeFromData MintingPolarity where
    {-# INLINABLE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = bool Mint Burn . unsafeFromBuiltinData

instance ToData MintingPolarity where
    {-# INLINABLE toBuiltinData #-}
    toBuiltinData Mint = toBuiltinData False
    toBuiltinData Burn = toBuiltinData True