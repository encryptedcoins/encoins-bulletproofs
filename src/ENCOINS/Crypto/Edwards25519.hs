{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Crypto.Edwards25519 where

import           Data.Bits                 (Bits (..))
import           Data.Bool                 (bool)
import           PlutusTx.Prelude
import           Prelude                   (seq)
import qualified Prelude                   as Haskell

import           ENCOINS.Crypto.Field      (FiniteField (..), Field (..), expField, fromFieldElement, toFieldElement)
import           PlutusTx.Extra.ByteString (toBytes, byteStringToInteger)
import           PlutusTx.Extra.Prelude    (replicate)

----------------------------------------------------------------------------------

type ExtendedPoint = (Field Ed25519, Field Ed25519, Field Ed25519, Field Ed25519)

addPoints :: ExtendedPoint -> ExtendedPoint -> ExtendedPoint
addPoints (pX, pY, pZ, pT) (qX, qY, qZ, qT) = (e*f, g*h, f*g, e*h)
  where
    a = (pY-pX) * (qY-qX)
    b = (pY+pX) * (qY+qX)
    c = F 2 * pT * qT * curveD
    d = F 2 * pZ * qZ
    e = b-a
    f = d-c
    g = d+c
    h = b+a

multiplyPoint :: Field Ed25519Field -> ExtendedPoint -> ExtendedPoint
multiplyPoint f = loop 255 (zero, one, one, zero)
  where
    s = fromFieldElement f
    loop !i !acc !pP
        | i Haskell.< 0 = pP `seq` acc
        | testBit s i   = loop (i Haskell.- 1) (addPoints acc pP)  (addPoints pP pP)
        | otherwise     = loop (i Haskell.- 1) (addPoints acc acc) (addPoints acc pP)

type CompressedPoint = BuiltinByteString

-- | Given y and the sign of x, recover x
recoverX :: Field Ed25519 -> Bool -> Field Ed25519
recoverX y xSign = x''
  where
    x2 = (y*y - one) * inv (curveD*y*y + one)
    x = expField x2 ((p+3) `divide` 8)

    x'
      | (x*x - x2) /= zero = x * f
      | otherwise          = x

    x''
      | even (fromFieldElement x') == xSign = zero - x'
      | otherwise                           = x'

    f :: Field Ed25519
    !f = expField (F 2) ((p-1) `divide` 4)

padBytes :: Integer -> BuiltinByteString -> BuiltinByteString
padBytes n bs =
    let m = n - lengthOfByteString bs
        b = consByteString zero emptyByteString
    in bool bs (foldl appendByteString bs $ replicate m b) $ m > 0

compressPoint :: ExtendedPoint -> CompressedPoint
compressPoint (pX, pY, pZ, _) =
    padBytes 32 $ toBytes (y .|. ((x .&. 0x1) `shiftL` 255))
  where
    zinv = inv pZ
    x = fromFieldElement $ pX * zinv
    y = fromFieldElement $ pY * zinv

decompressPoint :: CompressedPoint -> ExtendedPoint
decompressPoint bs =
    let cy    = byteStringToInteger bs
        xSign = testBit cy 255
        y     = toFieldElement $ clearBit cy 255
        x     = recoverX y xSign
     in (x, y, one, x*y)

------------------------- Edwards25519 constants -------------------------------

p :: Integer
p = 57896044618658097711785492504343953926634992332820282019728792003956564819949

data Ed25519 = Ed25519

instance Semigroup Ed25519 where
  (<>) = const . const $ Ed25519

instance Monoid Ed25519 where
  mempty = Ed25519

instance FiniteField Ed25519 where
  fieldPrime = const p

q :: Integer
q = 7237005577332262213973186563042994240857116359379907606001950938285454250989

data Ed25519Field = Ed25519Field

instance Semigroup Ed25519Field where
  (<>) = const . const $ Ed25519Field

instance Monoid Ed25519Field where
  mempty = Ed25519Field

instance FiniteField Ed25519Field where
  fieldPrime = const q

-- | Curve constant d
curveD :: Field Ed25519
curveD = F 37095705934669439343138083508754565189542113879843219016388785533085940283555

pG :: ExtendedPoint
pG = (F 15112221349535400772501151409588531511454012693041857206046113283949847762202,
      F 46316835694926478169428394003475163141307993866256225615783033603165251855960,
      F 1,
      F 46827403850823179245072216630277197565144205554125654976674165829533817101731)