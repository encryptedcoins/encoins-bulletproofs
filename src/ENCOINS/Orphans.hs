{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE FlexibleInstances             #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE NoImplicitPrelude             #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE TypeFamilies                  #-}

{-# OPTIONS_GHC -Wno-orphans               #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module ENCOINS.Orphans where

import           PlutusTx                          (unstableMakeIsData)

import           ENCOINS.BaseTypes
import           ENCOINS.Bulletproofs
import           ENCOINS.Crypto.Curve
import           ENCOINS.Crypto.Field

unstableMakeIsData ''Field
unstableMakeIsData ''BLS12381
unstableMakeIsData ''BLS12381Field
unstableMakeIsData ''GroupElement
unstableMakeIsData ''MintingPolarity
unstableMakeIsData ''BulletproofSetup
unstableMakeIsData ''Secret
unstableMakeIsData ''Randomness
unstableMakeIsData ''Input
unstableMakeIsData ''Proof