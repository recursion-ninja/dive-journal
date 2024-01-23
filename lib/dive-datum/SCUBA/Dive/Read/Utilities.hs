{-# LANGUAGE Safe #-}

module SCUBA.Dive.Read.Utilities (
    pChar,
    pProperString,
) where

import Control.Applicative (many)
import Data.List.NonEmpty (NonEmpty (..))
import Text.ParserCombinators.ReadPrec


pChar ∷ Char → ReadPrec ()
pChar c =
    get >>= \case
        v | v == c → pure ()
        _ → pfail


pProperString ∷ (NonEmpty Char → a) → ReadPrec a
pProperString con =
    many get >>= \case
        [] → pfail
        x : xs → pure . con $ x :| xs
