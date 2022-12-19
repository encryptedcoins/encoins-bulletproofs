{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module ENCOINS.Crypto.Curve where

import           Control.Monad.Random  (StdGen, MonadRandom (..), runRand, mkStdGen)
import           PlutusTx.Prelude
import           Prelude               ((^))

import           ENCOINS.Crypto.Field

class FiniteField c => Curve c where
    aCurveCoef :: Field c
    bCurveCoef :: Field c
    gen        :: Point c

----------------------------------------------------------------------------------

--- We will represent points on the curve as (x, y) tuples.
type Point c = (Field c, Field c)

-- This function adds two points on an elliptic curve.
addPoints :: Curve c => Point c -> Point c -> Point c
addPoints (x1, y1) (x2, y2)
  | x1 == x2 && y1 == y2 = pointDouble (x1, y1)
  | otherwise            = pointAdd (x1, y1) (x2, y2)

-- This function doubles a point on an elliptic curve.
pointDouble :: Curve c => Point c -> Point c
pointDouble (x, y)
  | (x, y) == (F 0, F 0) = (F 0, F 0)
  | otherwise = (x3, y3)
  where
    xx = x * x
    s  = (xx + xx + xx + aCurveCoef) * inv (y + y)
    x3 = s * s - x - x
    y3 = s * (x - x3) - y

-- This function adds two distinct points on an elliptic curve.
pointAdd :: Curve c => Point c -> Point c -> Point c
pointAdd (F 0, F 0) p = p
pointAdd p (F 0, F 0) = p
pointAdd (x1, y1) (x2, y2)
  | x1 == x2        = (F 0, F 0)
  | otherwise       = (x3, y3)
  where
    s = (y1 - y2) * inv (x1 - x2)
    x3 = s * s - x1 - x2
    y3 = s * (x1 - x3) - y1

pointMul :: Curve c => Point c -> Integer -> Point c
pointMul (x, y) n
  | n < 0             = pointMul (x, negate y) (-n)
  | n == 0            = (F 0, F 0)
  | n == 1            = (x, y)
  | n `modulo` 2 == 0 = (x', y')
  | otherwise         = pointAdd (x, y) (x', y')
  where
    (x', y') = pointMul (pointDouble (x, y)) (n `divide` 2)

fromX :: forall c . Curve c => Field c -> Maybe (Point c)
fromX x = do
  y <- squareRoot $ (x * x + aCurveCoef) * x + bCurveCoef
  pure (x, y)

----------------------------------------------------------------------------------

-- Jacobian coordinates
type PointJ c = (Field c, Field c, Field c)

-- This function adds two points on an elliptic curve.
addPointsJ :: Curve c => Point c -> Point c -> Point c
addPointsJ (x1, y1) (x2, y2)
  | x1 == x2 && y1 == y2 = fromJ $ pointDoubleJ $ toJ (x1, y1)
  | otherwise            = fromJ $ pointAddJ (toJ (x1, y1)) (toJ (x2, y2))

-- This function doubles a point on an elliptic curve.
pointDoubleJ :: Curve c => (Field c, Field c, Field c) -> (Field c, Field c, Field c)
pointDoubleJ (x1, y1, z1) = (x3, y3, z3)
  where
    two = one + one
    three = two + one
    eight = three + three + two

    a    = aCurveCoef
    xx   = x1 * x1
    yy   = y1 * y1
    yyyy = yy * yy
    zz   = z1 * z1
    xy   = x1 + yy
    yz   = y1 + z1
    s    = two * (xy * xy - xx - yyyy)
    m    = three * xx + a * zz * zz
    t    = m * m - two * s
    x3   = t
    y3   = m * (s - t) - eight * yyyy
    z3   = yz * yz - yy - zz

-- This function adds two distinct points on an elliptic curve.
pointAddJ :: Curve c => PointJ c -> PointJ c -> PointJ c
pointAddJ (x1, y1, z1) (x2, y2, z2)
          | z1 == zero = (x2, y2, z2)
          | z2 == zero = (x1, y1, z1)
          | x1 == x2   = (one, one, zero)
          | otherwise  = (x3, y3, z3)
  where
    two = one + one
    z1z1 = z1 * z1
    z2z2 = z2 * z2
    z1z2 = z1 + z2
    u1   = x1 * z2z2
    u2   = x2 * z1z1
    s1   = y1 * z2 * z2z2
    s2   = y2 * z1 * z1z1
    h    = u2 - u1
    h2   = two * h
    i    = h2 * h2
    j    = h * i
    r    = two * (s2 - s1)
    v    = u1 * i
    x3   = r * r - j - two * v
    y3   = r * (v - x3) - two * s1 * j
    z3   = (z1z2 * z1z2 - z1z1 - z2z2) * h

pointMulJ :: Curve c => PointJ c -> Integer -> PointJ c
pointMulJ (x, y, z) n
  | n < 0             = pointMulJ (x, negate y, z) (-n)
  | n == 0            = (F 1, F 1, F 0)
  | n == 1            = (x, y, z)
  | n `modulo` 2 == 0 = (x', y', z')
  | otherwise         = pointAddJ (x, y, z) (x', y', z')
  where
    (x', y', z') = pointMulJ (pointDoubleJ (x, y, z)) (n `divide` 2)

toJ :: Point c -> (Field c, Field c, Field c)
toJ (F 0, F 0) = (F 1, F 1, F 0)
toJ (x, y) = (x, y, F 1)

fromJ :: FiniteField c => (Field c, Field c, Field c) -> Point c
fromJ (x, y, z)
    | z == zero = (F 0, F 0)
    | otherwise = let zz = z * z
      in (x * inv zz, y * inv ((z * zz)))

------------------------ Square root computation -------------------------------

-- This function computes x^y mod p.
pow :: FiniteField c => Field c -> Integer -> Field c
pow a n
        | a == zero = zero
        | n == 0    = one
        | n <  0    = pow (inv a) (negate n)
        | otherwise = if r == 1 then a * pow aa q else pow aa q
    where
        (q, r) = quotRem n 2
        aa     = a * a

-- | Check if an element is a quadratic nonresidue.
isQNR :: forall c . FiniteField c => Field c -> Bool
isQNR a = a == zero || p /= 2 && pow a (p `divide` 2) /= one
  where p = fieldPrime (mempty :: c)

-- Get a random quadratic nonresidue.
getQNR :: forall c . FiniteField c => Maybe (Field c)
getQNR
  | p == 2    = Nothing
  | otherwise = Just $ F $ getQNR' $ runRand getRandom $ mkStdGen 0
  where
    p = fieldPrime (mempty :: c)
    getQNR' :: (Integer, StdGen) -> Integer
    getQNR' (x, g)
      | x /= zero && isQNR (F x :: Field c) = x
      | otherwise                           = getQNR' $ runRand getRandom g

-- Factor the order @p - 1@ to get @q@ and @s@ such that @p - 1 = q2^s@.
factorOrder :: Integer -> (Integer, Integer)
factorOrder p = factorOrder' (p - 1, 0)
  where
    factorOrder' :: (Integer, Integer) -> (Integer, Integer)
    factorOrder' qs@(q, s)
      | modulo q 2 == 1 = qs
      | otherwise       = factorOrder' (divide q 2, s + 1)

-- Get a square root of @n@ with the Tonelli-Shanks algorithm.
squareRoot :: forall c . FiniteField c => Field c -> Maybe (Field c)
squareRoot (F 0) = Just zero
squareRoot n
  | p == 2       = Just $ F 1
  | isQNR n      = Nothing
  | otherwise    = case (factorOrder p, getQNR) of
  ((q, s), Just z) -> let zq  = pow z q
                          nq  = pow n $ q `divide` 2
                          nnq = n * nq
                      in loop s zq (nq * nnq) nnq
  _                -> error ()
  where
    p = fieldPrime (mempty :: c)
    loop :: Integer -> Field c -> Field c -> Field c -> Maybe (Field c)
    loop _ _ (F 0) _ = Just zero
    loop _ _ (F 1) r = Just r
    loop m c t r = let i  = least t 0
                       b  = pow c $ 2^(m - i - 1)
                       b2 = b * b
                   in loop i b2 (t * b2) (r * b)
      where
        least :: Field c -> Integer -> Integer
        least (F 1) j = j
        least ti    j = least (ti * ti) (j + 1)

------------------------- Concrete curves -------------------------------

data BLS12381 = BLS12381

instance Semigroup BLS12381 where
  (<>) = const . const $ BLS12381

instance Monoid BLS12381 where
  mempty = BLS12381

instance FiniteField BLS12381 where
  fieldPrime = const 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

instance Curve BLS12381 where
  aCurveCoef = F 0
  bCurveCoef = F 4
  gen = (F 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb,
    F 0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1)

data BLS12381Field = BLS12381Field

instance Semigroup BLS12381Field where
  (<>) = const . const $ BLS12381Field

instance Monoid BLS12381Field where
  mempty = BLS12381Field

instance FiniteField BLS12381Field where
  fieldPrime = const 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001