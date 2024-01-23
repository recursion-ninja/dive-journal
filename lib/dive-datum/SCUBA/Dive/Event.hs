{-# LANGUAGE TemplateHaskell #-}

module SCUBA.Dive.Event (
    -- * DiveEvent
    DiveEvent (DiveEvent),

    -- ** Data-types
    DiveDate (..),
    DiveDay (),
    DiveHour (),
    DiveSiteName (),
    DiveCity (),
    DiveCountry (),
    DiveCoordinates (),

    -- ** Lenses
    diveDate,
    diveSiteName,
    diveCity,
    diveCountry,
    diveCoordinates,
) where

import Control.DeepSeq
import Data.Bifunctor (first)
import Data.Data
import Data.Foldable (toList)
import Data.List (inits)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Data.String
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, readPTime)
import Data.Time.LocalTime (TimeOfDay (..))
import GHC.Generics (Generic)
import Geodetics.Geodetic
import Lens.Micro ((^.))
import Lens.Micro.TH
import Lens.Micro.Type (Lens')
import SCUBA.Dive.Read.Utilities
import SCUBA.Entry.Class
import Text.Pandoc.Definition
import Text.ParserCombinators.ReadPrec
import Text.Read (Lexeme (Punc), Read (..), lexP, readListPrecDefault)


newtype DiveTitle = DiveTitle (NonEmpty Char)


-- | @since 0.1.0
deriving stock instance Data DiveTitle


-- | @since 0.1.0
deriving stock instance Eq DiveTitle


-- | @since 0.1.0
deriving stock instance Generic DiveTitle


-- | @since 0.1.0
instance IsString DiveTitle where
    fromString = \case
        [] → error "'DiveTitle' cannot be an empty string"
        x : xs → DiveTitle $ x :| xs


-- | @since 0.1.0
deriving anyclass instance NFData DiveTitle


-- | @since 0.1.0
deriving stock instance Ord DiveTitle


-- | @since 0.1.0
instance Read DiveTitle where
    {-# INLINEABLE readPrec #-}
    readPrec = pProperString DiveTitle


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveTitle where
    show (DiveTitle chars) = toList chars


newtype DiveDay = DiveDay Day


-- | @since 0.1.0
deriving stock instance Data DiveDay


-- | @since 0.1.0
deriving stock instance Eq DiveDay


-- | @since 0.1.0
deriving stock instance Generic DiveDay


-- | @since 0.1.0
deriving anyclass instance NFData DiveDay


-- | @since 0.1.0
deriving stock instance Ord DiveDay


-- | @since 0.1.0
instance Read DiveDay where
    {-# INLINEABLE readPrec #-}
    readPrec = DiveDay <$> readPrec


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveDay where
    show (DiveDay day) = show day


newtype DiveHour = DiveHour TimeOfDay


-- | @since 0.1.0
deriving stock instance Data DiveHour


-- | @since 0.1.0
deriving stock instance Eq DiveHour


-- | @since 0.1.0
deriving stock instance Generic DiveHour


-- | @since 0.1.0
deriving anyclass instance NFData DiveHour


-- | @since 0.1.0
deriving stock instance Ord DiveHour


-- | @since 0.1.0
instance Read DiveHour where
    {-# INLINEABLE readPrec #-}
    readPrec =
        fmap DiveHour . readP_to_Prec . const $
            readPTime True defaultTimeLocale "%R"


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveHour where
    show (DiveHour hour) = formatTime defaultTimeLocale "%R" hour


newtype DiveDate = DiveDate (DiveDay, DiveHour)


-- | @since 0.1.0
deriving stock instance Data DiveDate


-- | @since 0.1.0
deriving stock instance Eq DiveDate


-- | @since 0.1.0
deriving stock instance Generic DiveDate


-- | @since 0.1.0
deriving anyclass instance NFData DiveDate


-- | @since 0.1.0
deriving stock instance Ord DiveDate


-- | @since 0.1.0
instance Read DiveDate where
    {-# INLINEABLE readPrec #-}
    readPrec = do
        d ← readPrec
        Punc "@" ← lexP
        h ← readPrec
        pure $ DiveDate (d, h)


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveDate where
    show (DiveDate (day, hour)) = unwords [show day, "@", show hour]


newtype DiveSiteName = DiveSiteName (NonEmpty Char)


-- | @since 0.1.0
deriving stock instance Data DiveSiteName


-- | @since 0.1.0
deriving stock instance Eq DiveSiteName


-- | @since 0.1.0
deriving stock instance Generic DiveSiteName


-- | @since 0.1.0
instance IsString DiveSiteName where
    fromString = \case
        [] → error "'DiveSiteName' cannot be an empty string"
        x : xs → DiveSiteName $ x :| xs


-- | @since 0.1.0
deriving anyclass instance NFData DiveSiteName


-- | @since 0.1.0
deriving stock instance Ord DiveSiteName


-- | @since 0.1.0
instance Read DiveSiteName where
    {-# INLINEABLE readPrec #-}
    readPrec = pProperString DiveSiteName


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveSiteName where
    show (DiveSiteName chars) = toList chars


newtype DiveCity = DiveCity (NonEmpty Char)


-- | @since 0.1.0
deriving stock instance Data DiveCity


-- | @since 0.1.0
deriving stock instance Eq DiveCity


-- | @since 0.1.0
deriving stock instance Generic DiveCity


-- | @since 0.1.0
instance IsString DiveCity where
    fromString = \case
        [] → error "'DiveCity' cannot be an empty string"
        x : xs → DiveCity $ x :| xs


-- | @since 0.1.0
deriving anyclass instance NFData DiveCity


-- | @since 0.1.0
deriving stock instance Ord DiveCity


-- | @since 0.1.0
instance Read DiveCity where
    {-# INLINEABLE readPrec #-}
    readPrec = pProperString DiveCity


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveCity where
    show (DiveCity city) = toList city


newtype DiveCountry = DiveCountry (NonEmpty Char)


-- | @since 0.1.0
deriving stock instance Data DiveCountry


-- | @since 0.1.0
deriving stock instance Eq DiveCountry


-- | @since 0.1.0
deriving stock instance Generic DiveCountry


-- | @since 0.1.0
instance IsString DiveCountry where
    fromString = \case
        [] → error "'DiveCountry' cannot be an empty string"
        x : xs → DiveCountry $ x :| xs


-- | @since 0.1.0
deriving anyclass instance NFData DiveCountry


-- | @since 0.1.0
deriving stock instance Ord DiveCountry


-- | @since 0.1.0
instance Read DiveCountry where
    {-# INLINEABLE readPrec #-}
    readPrec = pProperString DiveCountry


    readListPrec = readListPrecDefault


-- | @since 0.1.0
instance Show DiveCountry where
    show (DiveCountry chars) = toList chars


newtype DiveCoordinates = DiveCoordinates (Geodetic WGS84)


-- | @since 0.1.0
instance Eq DiveCoordinates where
    (DiveCoordinates lhs) == (DiveCoordinates rhs) =
        let same ∷ (Eq a) ⇒ (Geodetic WGS84 → a) → Bool
            same f = f lhs == f rhs
        in  same latitude && same longitude


instance Read DiveCoordinates where
    {-# INLINEABLE readsPrec #-}
    readsPrec _ =
        let tryRead :: (String, String) -> Maybe (DiveCoordinates, String)
            tryRead (str, rest) =
                (\x -> (DiveCoordinates x, rest)) <$> readGroundPosition WGS84 str

            readerS :: String -> [(DiveCoordinates, String)]
            readerS str = mapMaybe tryRead $ zipWith (\i _ -> splitAt i str) [1 ..] str  
        in  readerS
        

    readListPrec = readListPrecDefault


instance Show DiveCoordinates where
    show (DiveCoordinates geo) =  unwords
        [ showAngle $ latitude geo
        , showAngle $ longitude geo
        ]


data DiveEvent = DiveEvent
    { _diveTitle :: DiveTitle
    , _diveDate ∷ DiveDate
    , _diveSiteName ∷ DiveSiteName
    , _diveCity ∷ DiveCity
    , _diveCountry ∷ DiveCountry
    , _diveCoordinates ∷ DiveCoordinates
    }


-- | @since 0.1.0
deriving stock instance Eq DiveEvent


-- | @since 0.1.0
instance NFData DiveEvent where
    rnf (DiveEvent a b c d e f) =
        f `seq` rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e


-- | @since 0.1.0
instance Ord DiveEvent where
    compare = comparing _diveDate


-- | @since 0.1.0
deriving stock instance Show DiveEvent


instance ToJournalSection DiveEvent where
    toJournalSection event =
        let header = pure . pandocDiveTitle $ event ^. diveTitle 
            defKVP =
                [ pandocDiveDate $ event ^. diveDate
                , pandocDiveLocation
                    (event ^. diveSiteName)
                    (event ^. diveCity)
                    (event ^. diveCountry)
                    (event ^. diveCoordinates)
                ]
        in  defineJournalSection 0 header defKVP


pandocDiveTitle ∷ DiveTitle → Inline
pandocDiveTitle (DiveTitle chars) = Str . fromString $ toList chars


pandocDiveDate ∷ DiveDate → ([Inline], [[Block]])
pandocDiveDate date =
    let key = toPandocKey "Dive Date"
        val = [toPandocVal $ show date]
    in  (key, val)


pandocDiveLocation ∷ DiveSiteName -> DiveCity → DiveCountry → DiveCoordinates → ([Inline], [[Block]])
pandocDiveLocation dSite dCity dCountry dCoordinates =
    let txt ∷ (Show a) ⇒ a → [Block]
        txt = toPandocVal . show
        key = toPandocKey "Dive Location"
        val =
            [ txt dSite
            , toPandocVal $ show dCity <> ", " <> show dCountry
            , txt dCoordinates
            ]
    in  (key, val)


{-# INLINE diveCity #-}
diveCity :: Lens' DiveEvent DiveCity
diveCity
  f_adZf
  (DiveEvent x1_adZg x2_adZh x3_adZi x4_adZj x5_adZk x6_adZl)
  = fmap
      (\ y1_adZm
         -> DiveEvent x1_adZg x2_adZh x3_adZi y1_adZm x5_adZk x6_adZl)
      (f_adZf x4_adZj)


{-# INLINE diveCoordinates #-}
diveCoordinates :: Lens' DiveEvent DiveCoordinates
diveCoordinates
  f_adZn
  (DiveEvent x1_adZo x2_adZp x3_adZq x4_adZr x5_adZs x6_adZt)
  = fmap
      (\ y1_adZu
         -> DiveEvent x1_adZo x2_adZp x3_adZq x4_adZr x5_adZs y1_adZu)
      (f_adZn x6_adZt)


{-# INLINE diveCountry #-}
diveCountry :: Lens' DiveEvent DiveCountry
diveCountry
  f_adZv
  (DiveEvent x1_adZw x2_adZx x3_adZy x4_adZz x5_adZA x6_adZB)
  = fmap
      (\ y1_adZC
         -> DiveEvent x1_adZw x2_adZx x3_adZy x4_adZz y1_adZC x6_adZB)
      (f_adZv x5_adZA)

{-# INLINE diveDate #-}
diveDate :: Lens' DiveEvent DiveDate
diveDate
  f_adZD
  (DiveEvent x1_adZE x2_adZF x3_adZG x4_adZH x5_adZI x6_adZJ)
  = fmap
      (\ y1_adZK
         -> DiveEvent x1_adZE y1_adZK x3_adZG x4_adZH x5_adZI x6_adZJ)
      (f_adZD x2_adZF)


{-# INLINE diveSiteName #-}
diveSiteName :: Lens' DiveEvent DiveSiteName
diveSiteName
  f_adZL
  (DiveEvent x1_adZM x2_adZN x3_adZO x4_adZP x5_adZQ x6_adZR)
  = fmap
      (\ y1_adZS
         -> DiveEvent x1_adZM x2_adZN y1_adZS x4_adZP x5_adZQ x6_adZR)
      (f_adZL x3_adZO)


{-# INLINE diveTitle #-}
diveTitle :: Lens' DiveEvent DiveTitle
diveTitle
  f_adZT
  (DiveEvent x1_adZU x2_adZV x3_adZW x4_adZX x5_adZY x6_adZZ)
  = fmap
      (\ y1_ae00
         -> DiveEvent y1_ae00 x2_adZV x3_adZW x4_adZX x5_adZY x6_adZZ)
      (f_adZT x1_adZU)
