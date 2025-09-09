{-# LANGUAGE Safe #-}

module SCUBA.Dive.Read.Utilities (
    pChar,
    pIdentifier,
    pLexUpper,
    pProperString,
) where

import Control.Applicative (many)
import Data.Char (toUpper)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Text.ParserCombinators.ReadPrec
import Text.Read (lexP)
import Text.Read.Lex (Lexeme (Ident))


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


pIdentifier :: String -> ReadPrec ()
pIdentifier label =
    let f = fmap toUpper
    in  lexP >>= \case
            Ident found | f label == f found -> pure ()
            _ -> pfail


pLexUpper :: ReadPrec Lexeme
pLexUpper = lexP <&> \case
    Ident label -> Ident $ toUpper <$> label
    val -> val
