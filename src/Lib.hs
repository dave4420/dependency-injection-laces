module Lib
( Inject()
, inject
, Dependable()
, use
, Module()
, DependencyError(..)
, dependencyOrThrow
, dependencyMay
, dependencyWhy
)
where

import Control.Exception
import Data.Dynamic
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid


newtype Inject a = Inject a
  deriving Typeable

inject :: a -> Inject a
inject = Inject

unInject :: Inject a -> a
unInject (Inject x) = x

class Typeable a => Dependable a where
  depend :: a -> (TypeRep, [TypeRep])

instance Typeable a => Dependable (Inject a) where
  depend a = (typeRep a, [])

instance (Typeable i, Dependable o) => Dependable (i -> o) where
  depend f = (baseRep, typeRep (Flip f) : paramReps) where
    (baseRep, paramReps) = depend (f undefined)

newtype Flip a b c = Flip (a c b)

use :: Dependable a => a -> Module
use factory = Module $ M.singleton resultRep [Injection (toDyn factory) paramReps] where
  (resultRep, paramReps) = depend factory


newtype Module = Module (M.Map TypeRep [Injection])

data Injection = Injection
  { injectionBase :: Dynamic
  , injectionDependencies :: [TypeRep]
  }

data DependencyError = MissingDependency TypeRep | DuplicateDependency TypeRep
  deriving (Eq, Show)

data DependencyException = DependencyException [DependencyError]
  deriving (Show, Typeable)
instance Exception DependencyException

instance Monoid Module where
  mempty = Module mempty
  Module x `mappend` Module y = Module (M.unionWith (<>) x y)

dependencyOrThrow :: Typeable a => Module -> a
dependencyOrThrow module' = case dependencyWhy module' of
  Left errors -> throw (DependencyException errors)
  Right dependency -> dependency

dependencyMay :: Typeable a => Module -> Maybe a
dependencyMay module' = case dependencyWhy module' of
  Left _errors -> Nothing
  Right dependency -> Just dependency

dependencyWhy :: forall a. Typeable a => Module -> Either [DependencyError] a
dependencyWhy (Module moduleMap) = ret where
  ret :: Either [DependencyError] a
  ret = fmap (unInject . (`fromDyn` error "BUG in injectedWhy: wrong return type")) (dependency $ typeRep ret)
  dependency :: TypeRep -> Either [DependencyError] Dynamic
  dependency rep = case fromMaybe [] (M.lookup rep moduleMap) of
    []              -> Left [MissingDependency rep]
    [Injection{..}] -> foldr cons (Right injectionBase) injectionDependencies
    _               -> Left [DuplicateDependency rep]
  cons :: TypeRep -> Either [DependencyError] Dynamic -> Either [DependencyError] Dynamic
  cons xRep fDyn = apply <$> fDyn <+> dependency xRep
  apply :: Dynamic -> Dynamic -> Dynamic
  apply fDyn xDyn
      = fromMaybe (error "BUG in injectedWhy: incompatible types when injecting dependencies") (dynApply fDyn xDyn)

infixl 4 <+> -- same as <*>
(<+>) :: Monoid e => Either e (a -> b) -> Either e a -> Either e b
Left e <+> Left e' = Left (e <> e')
Left e <+> Right _ = Left e
Right _ <+> Left e' = Left e'
Right f <+> Right x = Right (f x)
