{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.BaseTypes where

import           Control.Monad                 (fail)
import           Data.Aeson                    (FromJSON (..), ToJSON (..))
import           Data.Bool                     (bool)
import           Data.ByteString               (ByteString)
import           Data.Text                     (Text)
import           Data.Functor                  ((<$>))
import           GHC.Generics                  (Generic)
import           PlutusTx.Prelude              hiding ((<$>))
import qualified Prelude                       as Haskell
import           Test.QuickCheck               (Arbitrary(..))
import           Text.Hex                      (encodeHex, decodeHex)

import           ENCOINS.Crypto.Edwards25519
import           ENCOINS.Crypto.Field          (Field, toFieldElement, fromFieldElement)
import           PlutusTx.Extra.ByteString     (ToBuiltinByteString(..), byteStringToInteger)

------------------------------------- Field Element --------------------------------------

type FieldElement = Field Ed25519Field

newtype FieldElementBytes = FieldElementBytes BuiltinByteString
    deriving (Haskell.Eq, Haskell.Show, Generic)

bytes2fe :: FieldElementBytes -> FieldElement
bytes2fe (FieldElementBytes bs) = toFieldElement $ byteStringToInteger bs

fe2bytes :: FieldElement -> FieldElementBytes
fe2bytes = FieldElementBytes . toBytes . fromFieldElement

instance ToJSON FieldElementBytes where
    toJSON (FieldElementBytes bs) = toJSON $ encodeHex $ fromBuiltin bs

instance FromJSON FieldElementBytes where
    parseJSON v = do
        mbs <- (decodeHex :: Text -> Maybe ByteString) <$> parseJSON v
        let mf = fmap (FieldElementBytes . toBuiltin) mbs
        maybe (fail $ "A valid hex string is expected: " ++ Haskell.show v) return mf

instance Eq FieldElementBytes where
    (==) (FieldElementBytes bs1) (FieldElementBytes bs2) = bs1 == bs2

instance ToBuiltinByteString FieldElementBytes where
    toBytes (FieldElementBytes bs) = bs

------------------------------------- Group Element --------------------------------------

newtype GroupElement = GroupElement CompressedPoint
    deriving (Haskell.Eq, Haskell.Show, Generic)

instance ToJSON GroupElement where
    toJSON (GroupElement bs) = toJSON $ encodeHex $ fromBuiltin bs

instance FromJSON GroupElement where
    parseJSON v = do
        mbs <- (decodeHex :: Text -> Maybe ByteString) <$> parseJSON v
        let mg = fmap toBuiltin mbs >>= toGroupElement
        maybe (fail $ "A valid Ed25519 hex string is expected: " ++ Haskell.show v) return mg

instance Eq GroupElement where
    (==) (GroupElement e1) (GroupElement e2) = e1 == e2

instance ToBuiltinByteString GroupElement where
    {-# INLINABLE toBytes #-}
    toBytes (GroupElement bs) = bs

instance Arbitrary GroupElement where
    arbitrary = groupExp groupGenerator <$> arbitrary

{-# INLINABLE toGroupElement #-}
toGroupElement :: BuiltinByteString -> Maybe GroupElement
toGroupElement bs = bool Nothing (Just $ GroupElement bs) (bs == bs')
    where bs' = compressPoint $ decompressPoint bs

{-# INLINABLE fromGroupElement #-}
fromGroupElement :: GroupElement -> BuiltinByteString
fromGroupElement (GroupElement bs) = bs

{-# INLINABLE groupIdentity #-}
groupIdentity :: GroupElement
groupIdentity = GroupElement $ compressPoint (zero, one, one, zero)

{-# INLINABLE groupGenerator #-}
groupGenerator :: GroupElement
groupGenerator = GroupElement $ compressPoint pG

{-# INLINABLE groupMul #-}
groupMul :: GroupElement -> GroupElement -> GroupElement
groupMul (GroupElement g1) (GroupElement g2) = GroupElement $ compressPoint $ addPoints (decompressPoint g1) (decompressPoint g2)

{-# INLINABLE groupExp #-}
groupExp :: GroupElement -> FieldElement -> GroupElement
groupExp (GroupElement g1) n = GroupElement $ compressPoint $ multiplyPoint n (decompressPoint g1)

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