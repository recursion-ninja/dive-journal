module SCUBA.Entry.Class (
    -- * Data-type
    JournalEntry (),

    -- ** Constructor
    includeSection,

    -- ** Finalizer
    pandocJournalEntry,

    -- * Type-class
    ToJournalSection (..),

    -- ** Associated data-type
    JournalSection (),

    -- ** Constructor
    defineJournalSection,

    -- *** Helper λs
    toPandocKey,
    toPandocVal,
) where

import Data.Data (Data)
import Data.Foldable (toList)
import Data.Foldable1 (foldMap1')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Merge.Strict (SimpleWhenMatched, SimpleWhenMissing, merge, preserveMissing', zipWithMatched)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Arg (..))
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Text.Pandoc.Definition


newtype JournalEntry = JournalEntry (Map Word (NonEmpty JournalSectionDatum))


-- | @since 0.1.0
deriving stock instance Data JournalEntry


-- | @since 0.1.0
deriving stock instance Generic JournalEntry


type SectionData = NonEmpty JournalSectionDatum


instance Semigroup JournalEntry where
    (JournalEntry lhs) <> (JournalEntry rhs) =
        let onlyKeyL ∷ SimpleWhenMissing Word SectionData SectionData
            onlyKeyL = preserveMissing'
            onlyKeyR ∷ SimpleWhenMissing Word SectionData SectionData
            onlyKeyR = preserveMissing'
            bothKeys ∷ SimpleWhenMatched Word SectionData SectionData SectionData
            bothKeys = zipWithMatched $ const (<>)
        in  JournalEntry $ merge onlyKeyL onlyKeyR bothKeys lhs rhs


{- The Word defines the sort ordering -}
newtype JournalSection = JournalSection (Arg Word JournalSectionDatum)


-- | @since 0.1.0
deriving stock instance Data JournalSection


-- | @since 0.1.0
deriving stock instance Generic JournalSection


{-
Block (№ 1): Header
Block (№ 2): DefinitionList
-}
newtype JournalSectionDatum = JournalSectionDatum (Block, Block)


-- | @since 0.1.0
deriving stock instance Data JournalSectionDatum


-- | @since 0.1.0
deriving stock instance Generic JournalSectionDatum


class ToJournalSection a where
    toJournalSection ∷ a → JournalSection


defineJournalSection
    ∷ Word
    -- ^ Sort ordering
    → [Inline]
    -- ^ Section name
    → [([Inline], [[Block]])]
    -- ^ Key/Value pairs of the section entries
    → JournalSection
defineJournalSection rank sectionTitle kvps =
    let headerLevel
            | rank == 0 = 1
            | otherwise = 2
    in  JournalSection . Arg rank $
            JournalSectionDatum
                (Header headerLevel nullAttr sectionTitle, DefinitionList kvps)


includeSection ∷ JournalSection → JournalEntry
includeSection (JournalSection (Arg key val)) = JournalEntry . Map.singleton key $ pure val


pandocJournalEntry ∷ JournalEntry → Pandoc
pandocJournalEntry (JournalEntry mapping) =
    let sectionBlocks (JournalSectionDatum (headB, listB)) = headB :| [listB]
    in  Pandoc nullMeta $ foldMap (toList . foldMap1' sectionBlocks) mapping


toPandocKey ∷ String → [Inline]
toPandocKey = pure . Strong . pure . Str . fromString . (<> ":")


toPandocVal ∷ String → [Block]
toPandocVal = pure . Plain . pure . Str . fromString
