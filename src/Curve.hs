module Curve where

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import Crypto.Random.Types (MonadRandom)

class (Show a) => Curve a where
  curve :: a -> ECC.Curve
  cc :: a -> ECC.CurveCommon
  a :: a -> Integer
  n :: a -> Integer
  g :: a -> ECC.Point
  h :: a -> Integer
  isPointValid :: a -> ECC.Point -> Bool
  pointMul :: a -> Integer -> ECC.Point -> ECC.Point
  pointAdd :: a -> ECC.Point -> ECC.Point -> ECC.Point
  pointNegate :: a -> ECC.Point -> ECC.Point
  pointDouble :: a -> ECC.Point -> ECC.Point
  generateKeys :: MonadRandom m => a -> m (ECDSA.PublicKey, ECDSA.PrivateKey)
  isPointAtInfinity :: a -> ECC.Point -> Bool
  pointAddTwoMuls :: a -> Integer -> ECC.Point -> Integer -> ECC.Point -> ECC.Point
  pointBaseMul :: a -> Integer -> ECC.Point