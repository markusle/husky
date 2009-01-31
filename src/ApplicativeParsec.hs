-------------------------------------------------------------------
-- 
-- small wrapper around Control.Applicative and Control.Monad
-- to allow applicative style parsing in Parsec
--
-- This is taken almost verbatim from RWH
--
-------------------------------------------------------------------

-- | this module provides a small wrapper around Control.Applicative 
-- and Control.Monad to allow applicative style parsing in Parsec.
-- This is taken almost verbatim from RWH.


module ApplicativeParsec ( module Control.Applicative
                         , module Text.ParserCombinators.Parsec
                         ) where


-- imports
import Control.Applicative
import Control.Monad (ap, MonadPlus (..))

-- hide operators we re-export
import Text.ParserCombinators.Parsec hiding (many,optional, (<|>))


-- | Applicative instance for Monad
instance Applicative (GenParser s a) where
  pure  = return
  (<*>) = ap


-- |Alternative instance for MonadPlus
instance Alternative (GenParser s a) where
  empty = mzero
  (<|>) = mplus
