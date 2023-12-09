module Common (readEntire, readMany, readSome) where

import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import qualified Data.Text as T (dropWhile, null)
import Data.Text.Read (Reader)

readEntire :: Reader a -> Text -> Either String a
readEntire reader input = do
    (a, t) <- reader input
    if T.null t then Right a else Left "incomplete read"

readMany :: Reader a -> Reader [a]
readMany reader = pure . readMany' id where
    readMany' k input =
        either (const (k [], input)) (uncurry $ readMany' . (.) k . (:)) .  reader $
        T.dropWhile isSpace input

readSome :: Reader a -> Reader (NonEmpty a)
readSome reader input = do
    (a, input') <- reader input
    first (a :|) <$> readMany reader input'
