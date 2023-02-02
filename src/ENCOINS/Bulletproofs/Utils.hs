{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Bulletproofs.Utils where

import           PlutusTx.Prelude

import           ENCOINS.BaseTypes
import           ENCOINS.Crypto.Field
import           PlutusTx.Extra.Prelude    (drop, replicate)
import           PlutusTx.Extra.ByteString (byteStringToInteger)

----------------------------------- Challenge ---------------------------------------

ge2fe :: GroupElement -> FieldElement
ge2fe = toFieldElement . byteStringToInteger . fromGroupElement

{-# INLINABLE challenge #-}
challenge :: [GroupElement] -> (FieldElement, FieldElement)
challenge gs = (f1, f2)
    where
        gs' = map (groupExp groupGenerator . ge2fe) gs
        f1  = ge2fe $ foldr groupMul groupIdentity gs'
        f2  = ge2fe $ groupExp groupGenerator f1

----------------------------------- Polynomials -------------------------------------

type Poly = ([FieldElement], [FieldElement])

polyProduct :: Poly -> Poly -> (FieldElement, FieldElement, FieldElement)
polyProduct (l0, l1) (r0, r1) = (c0, c1, c2)
    where
        c0 = sum $ zipWith (*) l0 r0
        c1 = sum $ zipWith (*) l1 r0 ++ zipWith (*) l0 r1
        c2 = sum $ zipWith (*) l1 r1

polyEvaluate :: Poly -> FieldElement -> [FieldElement]
polyEvaluate p x = zipWith (+) (fst p) (map (* x) (snd p))

----------------------------------- Conversions -------------------------------------

toBits :: FieldElement -> [Integer]
toBits (F a) = r : if q > 0 then toBits (F q) else []
    where (q, r) = divMod a 2

fromBits :: [Integer] -> [FieldElement]
fromBits = map F

padBits :: Integer -> [Integer] -> [Integer]
padBits _ [] = []
padBits k x
    | l > k     = x
    | otherwise = x ++ replicate (k-l) zero
  where l = length x

------------------------------------- Polarity --------------------------------------

{-# INLINABLE polarityToInteger #-}
polarityToInteger :: MintingPolarity -> Integer
polarityToInteger Mint = 1
polarityToInteger Burn = -1

{-# INLINABLE withPolarity #-}
withPolarity :: MintingPolarity -> FieldElement -> FieldElement
withPolarity p v = if p == Mint then v else negate v

----------------------------------- Arithmetics -------------------------------------

{-# INLINABLE powers #-}
powers :: FieldElement -> Integer -> [FieldElement]
powers _ 1 = [one]
powers e n = one : map (* e) (powers e (n-1))

{-# INLINABLE powersOfZ #-}
powersOfZ :: FieldElement -> Integer -> ([FieldElement], FieldElement)
powersOfZ z m = (\lst -> (take m lst, lst !! m)) $ drop 2 $ powers z (m+3)

powersOf2 :: Integer -> [FieldElement]
powersOf2 = powers (F 2)