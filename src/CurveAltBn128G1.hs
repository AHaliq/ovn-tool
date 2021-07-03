module CurveAltBn128G1 (CurveAltBn128G1) where

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import Curve as C

data CurveAltBn128G1 = CurveAltBn128G1 deriving (Show)

{-
ecc_a = 0
ecc_b = 3
ecc_g = (1,2)
ecc_n = 21888242871839275222246405745257275088548364400416034343698204186575808495617
ecc_h = 1
-}
curveAltBn128G1 :: ECC.Curve
curveAltBn128G1 =
  ECC.CurveFP $
    ECC.CurvePrime
      0x30644E72E131A029B85045B68181585D97816A916871CA8D3C208C16D87CFD47
      ECC.CurveCommon
        { ECC.ecc_a = 0x0,
          ECC.ecc_b = 0x3,
          ECC.ecc_g = ECC.Point 0x1 0x2,
          ECC.ecc_n = 0x30644E72E131A029B85045B68181585D2833E84879B9709143E1F593F0000001,
          ECC.ecc_h = 0x1
        }

instance C.Curve CurveAltBn128G1 where
  curve = const curveAltBn128G1
  cc = ECC.common_curve . curve
  a = ECC.ecc_a . cc
  n = ECC.ecc_n . cc
  g = ECC.ecc_g . cc
  h = ECC.ecc_h . cc
  isPointValid = ECC.isPointValid . curve
  pointMul = ECC.pointMul . curve
  pointAdd = ECC.pointAdd . curve
  pointNegate = ECC.pointNegate . curve
  pointDouble = ECC.pointDouble . curve
  generateKeys = ECC.generate . curve
  isPointAtInfinity = const ECC.isPointAtInfinity
  pointAddTwoMuls = ECC.pointAddTwoMuls . curve
  pointBaseMul = ECC.pointBaseMul . curve