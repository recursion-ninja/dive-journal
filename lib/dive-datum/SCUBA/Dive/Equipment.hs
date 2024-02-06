{-# LANGUAGE TemplateHaskell #-}

module SCUBA.Dive.Equipment (
    -- * DiveEvent
    DiveEquipment (),

    -- ** Data-types
    -- *** Weight
    DiveWeight (),
    DiveWeightQuantity (),
    DiveWeightUnit (),

    -- ** Coverings
    CoveringThickness(),
    DiveCoveringBody(),
    DiveCoveringHead(),

    -- ** Air Canister
    DiveAirMixture(..),
    DiveTankMaterial(..),
    
    -- ** Lenses
    diveAirMixture,
    diveCoveringBody,
    diveCoveringHead,
    diveTankMaterial,
    diveWeight,
) where

import Control.Applicative (optional)
import Control.DeepSeq
import Data.Ratio (denominator)
import Data.Data
import Data.Foldable (fold)
import Data.Functor (($>))
-- import Data.String
import GHC.Generics (Generic)
--import Lens.Micro ((^.))
--import Lens.Micro.TH
import Lens.Micro.Type (Lens')
import SCUBA.Dive.Read.Utilities
--import SCUBA.Entry.Class
--import Text.Pandoc.Definition
import Text.ParserCombinators.ReadPrec
import Text.Read (Read (..), lexP, readListPrecDefault)
import Text.Read.Lex (Lexeme (Ident, Number), numberToRational)

import Debug.Trace


-- | @since 0.1.0
deriving stock instance Data DiveWeightQuantity


-- | @since 0.1.0
deriving stock instance Eq DiveWeightQuantity


-- | @since 0.1.0
deriving stock instance Generic DiveWeightQuantity


-- | @since 0.1.0
deriving anyclass instance NFData DiveWeightQuantity


-- | @since 0.1.0
deriving stock instance Ord DiveWeightQuantity


-- | @since 0.1.0
instance Read DiveWeightQuantity where
    {-# INLINEABLE readPrec #-}
    readPrec = DiveWeightQuantity <$> readPrec


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveWeightQuantity where
    show (DiveWeightQuantity quant) = show quant


data DiveWeightUnit = Unit_Kilogram | Unit_Pounds


-- | @since 0.1.0
deriving stock instance Data DiveWeightUnit


-- | @since 0.1.0
deriving stock instance Eq DiveWeightUnit


-- | @since 0.1.0
deriving stock instance Generic DiveWeightUnit


-- | @since 0.1.0
deriving anyclass instance NFData DiveWeightUnit


-- | @since 0.1.0
deriving stock instance Ord DiveWeightUnit


-- | @since 0.1.0
instance Read DiveWeightUnit where
    {-# INLINEABLE readPrec #-}
    readPrec =
        let bar =
                pLexUpper >>= \case
                    Ident "KG" → pure Unit_Kilogram
                    _ → pfail

            psi =
                pLexUpper >>= \case
                    Ident "LBS" → pure Unit_Pounds
                    _ → pfail
        in  bar <++ psi


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveWeightUnit where
    show = \case
        Unit_Kilogram → "kg"
        Unit_Pounds → "lbs"


newtype DiveWeightQuantity = DiveWeightQuantity Word


newtype DiveWeight = DiveWeight (DiveWeightQuantity, DiveWeightUnit)


-- | @since 0.1.0
deriving stock instance Data DiveWeight


-- | @since 0.1.0
deriving stock instance Eq DiveWeight


-- | @since 0.1.0
deriving stock instance Generic DiveWeight


-- | @since 0.1.0
deriving anyclass instance NFData DiveWeight


-- | @since 0.1.0
deriving stock instance Ord DiveWeight


-- | @since 0.1.0
instance Read DiveWeight where
    {-# INLINEABLE readPrec #-}
    readPrec = fmap DiveWeight $ (,) <$> readPrec <*> readPrec


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveWeight where
    show (DiveWeight (quant, units)) = unwords [show quant, show units]



-- Milimeters; and a half unit?
data CoveringThickness = CoveringThickness Word Bool


-- | @since 0.1.0
deriving stock instance Data CoveringThickness


-- | @since 0.1.0
deriving stock instance Eq CoveringThickness


-- | @since 0.1.0
deriving stock instance Generic CoveringThickness


-- | @since 0.1.0
deriving anyclass instance NFData CoveringThickness


-- | @since 0.1.0
deriving stock instance Ord CoveringThickness


-- | @since 0.1.0
instance Read CoveringThickness where
    {-# INLINEABLE readPrec #-}
    readPrec =
        let numeric =
                lexP >>= \case
                    Number num ->
                       let rat = numberToRational num
                           top = truncate rat 
                       in  case denominator rat of
                               1 -> pure $ CoveringThickness top False
                               2 -> pure $ CoveringThickness top True
                               _ -> pfail
                    val -> trace ("numeric [WRONG]:\t" <> show val) pfail
        in  numeric <* optional (pIdentifier "mm")


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show CoveringThickness where
    show (CoveringThickness mms half) =
        let fractional
                | half      = ".5"
                | otherwise = ".0"
        in  fold [ show mms, fractional, "mm" ]


data DiveCoveringBody
    = WetsuitFull    DiveCoveringBody
    | WetsuitShorty  DiveCoveringBody
    | Rashguard


-- | @since 0.1.0
deriving stock instance Data DiveCoveringBody


-- | @since 0.1.0
deriving stock instance Eq DiveCoveringBody


-- | @since 0.1.0
deriving stock instance Generic DiveCoveringBody


-- | @since 0.1.0
deriving anyclass instance NFData DiveCoveringBody


-- | @since 0.1.0
deriving stock instance Ord DiveCoveringBody


-- | @since 0.1.0
instance Read DiveCoveringBody where
    {-# INLINEABLE readPrec #-}
    readPrec = choice
        [ pIdentifier "Wetsuit" >>= const (WetsuitFull <$> readPrec)
        , pIdentifier "Shorty"  >>= const (WetsuitShorty <$> readPrec)
        , pIdentifier "Rashguard" $> Rashguard
        ]

    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveCoveringBody where
    show = \case
        WetsuitFull thickness -> unwords [ "Wetsuit", show thickness ] 
        WetsuitShorty thickness -> unwords [ "Shorty", show thickness ]
        Rashguard -> "Rashguard"


data DiveCoveringHead
    = HeadHood CoveringThickness


-- | @since 0.1.0
deriving stock instance Data DiveCoveringHead


-- | @since 0.1.0
deriving stock instance Eq DiveCoveringHead


-- | @since 0.1.0
deriving stock instance Generic DiveCoveringHead


-- | @since 0.1.0
deriving anyclass instance NFData DiveCoveringHead


-- | @since 0.1.0
deriving stock instance Ord DiveCoveringHead


-- | @since 0.1.0
instance Read DiveCoveringHead where
    {-# INLINEABLE readPrec #-}
    readPrec = pIdentifier "Hood" *> (HeadHood <$> readPrec)

    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveCoveringHead where
    show = \case
         HeadHood thickness -> unwords [ "Hood", show thickness ] 


data DiveTankMaterial = Tank_Al | Tank_Fe


-- | @since 0.1.0
deriving stock instance Data DiveTankMaterial


-- | @since 0.1.0
deriving stock instance Eq DiveTankMaterial


-- | @since 0.1.0
deriving stock instance Generic DiveTankMaterial


-- | @since 0.1.0
deriving anyclass instance NFData DiveTankMaterial


-- | @since 0.1.0
deriving stock instance Ord DiveTankMaterial


-- | @since 0.1.0
instance Read DiveTankMaterial where
    {-# INLINEABLE readPrec #-}
    readPrec = choice
        [ pIdentifier "Al" $> Tank_Al
        , pIdentifier "Fe" $> Tank_Fe
        ]

    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveTankMaterial where
    show = \case
         Tank_Al -> "Al"
         Tank_Fe -> "Fe"


data DiveAirMixture
    = Air_Standard
    | Air_Enriched
    

-- | @since 0.1.0
deriving stock instance Data DiveAirMixture


-- | @since 0.1.0
deriving stock instance Eq DiveAirMixture


-- | @since 0.1.0
deriving stock instance Generic DiveAirMixture


-- | @since 0.1.0
deriving anyclass instance NFData DiveAirMixture


-- | @since 0.1.0
deriving stock instance Ord DiveAirMixture


-- | @since 0.1.0
instance Read DiveAirMixture where
    {-# INLINEABLE readPrec #-}
    readPrec = choice
        [ pIdentifier "Standard" $> Air_Standard
        , pIdentifier "Enriched" $> Air_Enriched
        ]

    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveAirMixture where
    show = \case
         Air_Standard -> "Standard"
         Air_Enriched -> "Enriched"


data DiveEquipment = DiveEquipment
    { _diveWeight :: DiveWeight
    , _diveCoveringBody :: DiveCoveringBody
    , _diveCoveringHead :: DiveCoveringHead
    , _diveAirMixture :: DiveAirMixture
    , _diveTankMaterial :: DiveTankMaterial
    }


-- | @since 0.1.0
deriving stock instance Data DiveEquipment


-- | @since 0.1.0
deriving stock instance Eq DiveEquipment


-- | @since 0.1.0
deriving stock instance Generic DiveEquipment


-- | @since 0.1.0
deriving anyclass instance NFData DiveEquipment


-- | @since 0.1.0
deriving stock instance Ord DiveEquipment


-- | @since 0.1.0
deriving stock instance Show DiveEquipment


diveAirMixture :: Lens' DiveEquipment DiveAirMixture
diveAirMixture
  f_auDf
  (DiveEquipment x1_auDg x2_auDh x3_auDi x4_auDj x5_auDk)
  = fmap
      (\ y1_auDl
         -> DiveEquipment x1_auDg x2_auDh x3_auDi y1_auDl x5_auDk)
      (f_auDf x4_auDj)
{-# INLINE diveAirMixture #-}
diveCoveringBody :: Lens' DiveEquipment DiveCoveringBody
diveCoveringBody
  f_auDm
  (DiveEquipment x1_auDn x2_auDo x3_auDp x4_auDq x5_auDr)
  = fmap
      (\ y1_auDs
         -> DiveEquipment x1_auDn y1_auDs x3_auDp x4_auDq x5_auDr)
      (f_auDm x2_auDo)
{-# INLINE diveCoveringBody #-}
diveCoveringHead :: Lens' DiveEquipment DiveCoveringHead
diveCoveringHead
  f_auDt
  (DiveEquipment x1_auDu x2_auDv x3_auDw x4_auDx x5_auDy)
  = fmap
      (\ y1_auDz
         -> DiveEquipment x1_auDu x2_auDv y1_auDz x4_auDx x5_auDy)
      (f_auDt x3_auDw)
{-# INLINE diveCoveringHead #-}
diveTankMaterial :: Lens' DiveEquipment DiveTankMaterial
diveTankMaterial
  f_auDA
  (DiveEquipment x1_auDB x2_auDC x3_auDD x4_auDE x5_auDF)
  = fmap
      (\ y1_auDG
         -> DiveEquipment x1_auDB x2_auDC x3_auDD x4_auDE y1_auDG)
      (f_auDA x5_auDF)
{-# INLINE diveTankMaterial #-}
diveWeight :: Lens' DiveEquipment DiveWeight
diveWeight
  f_auDH
  (DiveEquipment x1_auDI x2_auDJ x3_auDK x4_auDL x5_auDM)
  = fmap
      (\ y1_auDN
         -> DiveEquipment y1_auDN x2_auDJ x3_auDK x4_auDL x5_auDM)
      (f_auDH x1_auDI)
{-# INLINE diveWeight #-}




{-
instance ToJournalSection DiveEquipment where
    toJournalSection result =
        let header = pure . Str $ fromString "Dive Data"
            defKVP =
                [ pandocDiveTotalTime $ result ^. diveTotalTime
                , pandocDiveMaximumDepth $ result ^. diveMaximumDepth
                , pandocDiveWeight
                    (result ^. diveAirSupplyFinal)
                    (result ^. diveAirSupplyStart)
                ]
        in  defineJournalSection 4 header defKVP
-}

{-

pandocDiveWeight ∷ DiveWeight → DiveWeight → ([Inline], [[Block]])
pandocDiveWeight start final =
    let key = toPandocKey "Air Supply"
        val =
            [ toPandocVal $ show start
            , toPandocVal $ show final
            ]
    in  (key, val)


pandocDiveMaximumDepth ∷ DiveMaximumDepth → ([Inline], [[Block]])
pandocDiveMaximumDepth depth =
    let key = toPandocKey "Maximum Depth"
        val = [toPandocVal $ show depth]
    in  (key, val)


pandocDiveTotalTime ∷ DiveTotalTime → ([Inline], [[Block]])
pandocDiveTotalTime minutes =
    let key = toPandocKey "Total Time"
        val = [toPandocVal $ show minutes]
    in  (key, val)
-}
