module Parser
  ( Parser(..)
  , Input(..)
  , nextP
  , ifP
  , spanP
  , runParser
  , lift
  ) where

import Control.Monad.State (StateT(..), evalStateT, lift, get)
import Control.Applicative (Alternative(..))
import Control.Monad (mfilter)
import Data.List (uncons)
import Data.Kind (Type, Constraint)

type Parser :: Type -> Type -> Type
type Parser i = StateT i Maybe

runParser = runStateT

type Input :: (Type -> Type) -> Constraint
class Input f where
  inputNext :: f i -> Maybe (i, f i)

instance Input [] where
  inputNext = uncons

nextP :: Input f => Parser (f i) i
nextP = StateT inputNext

ifP :: Input f => (i -> Bool) -> Parser (f i) i
ifP predicate = mfilter predicate nextP

spanP :: Input f => (i -> Bool) -> Parser (f i) [i]
spanP = many . ifP
