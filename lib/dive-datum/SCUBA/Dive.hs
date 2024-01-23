{-# LANGUAGE TemplateHaskell #-}

module SCUBA.Dive (
    Dive (),
    DiveDepth (),
    DiveDuration (),
) where

import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.LocalTime (TimeOfDay)
import Geodetics.Geodetic
import Lens.Micro ((^.))
import Lens.Micro.TH
import Lens.Micro.Type (Lens')
import SCUBA.Dive.Event


data DiveEnvironment = DiveEnvironment
    { diveVisibility ∷ DiveVisibility
    , diveWaterCategory ∷ DiveWaterCategory
    , diveEntryMethod ∷ DiveEntryMethod
    }


data DiveEquipment = DiveEquipment
    { diveAirType ∷ AirMixture
    , diveAirFull ∷ AirPressure
    , diveAirDone ∷ AirPressure
    , diveWeights ∷ DiveWeights
    }


data Distance = Centimeters | Inches


{- |
The duration of the dive.
-}
newtype DiveDuration = DiveTime DiffTime


newtype DiveDepth = DiveDepth (Word, Distance)


data AirMixture
    = Standard
    | Nitrox


newtype AirPressure = AirPressure Rational


newtype DiveWeights = DiveWeights Rational


data DiveVisibility = Poor | Fair | Good | Excellent


data DiveWaterCategory
    = Ocean
    | Lake DiveWaterSalinity
    | Quarry DiveWaterSalinity
    | Aquarium DiveWaterSalinity


data DiveEntryMethod
    = Boat
    | Shore


data DiveWaterSalinity = FreshWater | SaltWater


data Dive = Dive
    { _diveEvent ∷ DiveEvent
    }


-- toJournalSection $ event ^. diveEvent

{-# INLINE diveEvent #-}
diveEvent ∷ Lens' Dive DiveEvent
diveEvent f_aoe6 (Dive x1_aoe7) =
    fmap (\y1_aoe8 → Dive y1_aoe8) (f_aoe6 x1_aoe7)

-- makeLenses ''Dive
