{-# LANGUAGE TemplateHaskell #-}

module SCUBA.Dive.Result (
    -- * DiveEvent
    DiveResult (),

    -- ** Data-types
    DiveAirSupply (),
    DiveAirUnit (),
    DiveMaximumDepth (),
    DiveTotalTime (),

    -- ** Lenses
    diveAirSupplyFinal,
    diveAirSupplyStart,
    diveMaximumDepth,
    diveTotalTime,
) where

import Control.DeepSeq
import Data.Char (toUpper)
import Data.Data
import Data.Foldable (fold)
import Data.String
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Lens.Micro.TH
import Lens.Micro.Type (Lens')
import SCUBA.Dive.Read.Utilities
import SCUBA.Entry.Class
import Text.Pandoc.Definition
import Text.ParserCombinators.ReadPrec
import Text.Read (Read (..), lexP, readListPrecDefault)
import Text.Read.Lex (Lexeme (Ident, Number), numberToRational)


data DiveAirUnit = Unit_BAR | Unit_PSI


-- | @since 0.1.0
deriving stock instance Data DiveAirUnit


-- | @since 0.1.0
deriving stock instance Eq DiveAirUnit


-- | @since 0.1.0
deriving stock instance Generic DiveAirUnit


-- | @since 0.1.0
deriving anyclass instance NFData DiveAirUnit


-- | @since 0.1.0
deriving stock instance Ord DiveAirUnit


-- | @since 0.1.0
instance Read DiveAirUnit where
    {-# INLINEABLE readPrec #-}
    readPrec =
        let bar =
                lexP >>= \case
                    Ident val | (toUpper <$> val) == "BAR" → pure Unit_BAR
                    _ → pfail

            psi =
                lexP >>= \case
                    Ident val | (toUpper <$> val) == "PSI" → pure Unit_PSI
                    _ → pfail
        in  bar <++ psi


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveAirUnit where
    show = \case
        Unit_BAR → "BAR"
        Unit_PSI → "PSI"


newtype DiveAirSupply = DiveAirSupply (Word, DiveAirUnit)


-- | @since 0.1.0
deriving stock instance Data DiveAirSupply


-- | @since 0.1.0
deriving stock instance Eq DiveAirSupply


-- | @since 0.1.0
deriving stock instance Generic DiveAirSupply


-- | @since 0.1.0
deriving anyclass instance NFData DiveAirSupply


-- | @since 0.1.0
deriving stock instance Ord DiveAirSupply


-- | @since 0.1.0
instance Read DiveAirSupply where
    {-# INLINEABLE readPrec #-}
    readPrec = fmap DiveAirSupply $ (,) <$> readPrec <*> readPrec


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveAirSupply where
    show (DiveAirSupply (quant, units)) = unwords [show quant, show units]


data DiveMaximumDepth
    = MaxDepth_Decimeter Word -- Decimeter precision (10⁻¹)
    | MaxDepth_FootInch Word Word


-- | @since 0.1.0
deriving stock instance Data DiveMaximumDepth


-- | @since 0.1.0
deriving stock instance Eq DiveMaximumDepth


-- | @since 0.1.0
deriving stock instance Generic DiveMaximumDepth


-- | @since 0.1.0
deriving anyclass instance NFData DiveMaximumDepth


-- | @since 0.1.0
deriving stock instance Ord DiveMaximumDepth


-- | @since 0.1.0
instance Read DiveMaximumDepth where
    {-# INLINEABLE readPrec #-}
    readPrec =
        let metric =
                let value =
                        lexP >>= \case
                            Number num → pure . truncate $ numberToRational num * 10
                            _ → pfail

                    suffix =
                        lexP >>= \case
                            Ident (val : rest) | toUpper val == 'M' → case rest of
                                "" → pure ()
                                "eter" → pure ()
                                "eters" → pure ()
                                _ → pfail
                            _ → pfail
                in  MaxDepth_Decimeter <$> (value <* suffix)

            imperial =
                let vFoot = readPrec <* pChar '\''
                    vInch = readPrec <* pChar '"'
                in  MaxDepth_FootInch <$> vFoot <*> vInch
        in  metric <++ imperial


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveMaximumDepth where
    show = \case
        MaxDepth_Decimeter deci →
            let (m, r) = deci `divMod` 10
            in  unwords [fold [show m, ".", show r], "m"]
        MaxDepth_FootInch f i →
            unwords [show f <> "'", show i <> "\""]


newtype DiveTotalTime = DiveTotalTime Word -- Minutes


-- | @since 0.1.0
deriving stock instance Data DiveTotalTime


-- | @since 0.1.0
deriving stock instance Eq DiveTotalTime


-- | @since 0.1.0
deriving stock instance Generic DiveTotalTime


-- | @since 0.1.0
deriving anyclass instance NFData DiveTotalTime


-- | @since 0.1.0
deriving stock instance Ord DiveTotalTime


-- | @since 0.1.0
instance Read DiveTotalTime where
    {-# INLINEABLE readPrec #-}
    readPrec =
        let suffix =
                lexP >>= \case
                    Ident (m : 'i' : 'n' : rest) | toUpper m == 'M' → case rest of
                        "" → pure ()
                        "ute" → pure ()
                        "utes" → pure ()
                        _ → pfail
                    _ → pfail
        in  DiveTotalTime <$> (readPrec <* suffix)


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveTotalTime where
    show (DiveTotalTime minutes) = unwords [show minutes, "min"]


data DiveResult = DiveResult
    { _diveTotalTime ∷ DiveTotalTime
    , _diveMaximumDepth ∷ DiveMaximumDepth
    , _diveAirSupplyStart ∷ DiveAirSupply
    , _diveAirSupplyFinal ∷ DiveAirSupply
    }


-- | @since 0.1.0
deriving stock instance Data DiveResult


-- | @since 0.1.0
deriving stock instance Eq DiveResult


-- | @since 0.1.0
deriving stock instance Generic DiveResult


-- | @since 0.1.0
deriving anyclass instance NFData DiveResult


-- | @since 0.1.0
deriving stock instance Ord DiveResult


-- | @since 0.1.0
deriving stock instance Show DiveResult


instance ToJournalSection DiveResult where
    toJournalSection result =
        let header = pure . Str $ fromString "Dive Data"
            defKVP =
                [ pandocDiveTotalTime $ result ^. diveTotalTime
                , pandocDiveMaximumDepth $ result ^. diveMaximumDepth
                , pandocDiveAirSupply
                    (result ^. diveAirSupplyFinal)
                    (result ^. diveAirSupplyStart)
                ]
        in  defineJournalSection 4 header defKVP


pandocDiveAirSupply ∷ DiveAirSupply → DiveAirSupply → ([Inline], [[Block]])
pandocDiveAirSupply start final =
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


{-# INLINE diveAirSupplyFinal #-}
diveAirSupplyFinal ∷ Lens' DiveResult DiveAirSupply
diveAirSupplyFinal
    f_adte
    (DiveResult x1_adtf x2_adtg x3_adth x4_adti) =
        fmap
            (\y1_adtj → DiveResult x1_adtf x2_adtg x3_adth y1_adtj)
            (f_adte x4_adti)


{-# INLINE diveAirSupplyStart #-}
diveAirSupplyStart ∷ Lens' DiveResult DiveAirSupply
diveAirSupplyStart
    f_adtk
    (DiveResult x1_adtl x2_adtm x3_adtn x4_adto) =
        fmap
            (\y1_adtp → DiveResult x1_adtl x2_adtm y1_adtp x4_adto)
            (f_adtk x3_adtn)


{-# INLINE diveMaximumDepth #-}
diveMaximumDepth ∷ Lens' DiveResult DiveMaximumDepth
diveMaximumDepth
    f_adtq
    (DiveResult x1_adtr x2_adts x3_adtt x4_adtu) =
        fmap
            (\y1_adtv → DiveResult x1_adtr y1_adtv x3_adtt x4_adtu)
            (f_adtq x2_adts)


{-# INLINE diveTotalTime #-}
diveTotalTime ∷ Lens' DiveResult DiveTotalTime
diveTotalTime f_adtw (DiveResult x1_adtx x2_adty x3_adtz x4_adtA) =
    fmap
        (\y1_adtB → DiveResult y1_adtB x2_adty x3_adtz x4_adtA)
        (f_adtw x1_adtx)
