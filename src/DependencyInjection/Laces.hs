module DependencyInjection.Laces
( Inject()
, inject
, unInject
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

data Dependencies = Dependencies
  { dependenciesTarget :: TypeRep
  , dependenciesDependencies :: [TypeRep]
  , dependenciesFinalise :: Dynamic
  }

class Typeable a => Dependable a where
  depend :: a -> Dependencies

instance Typeable a => Dependable (Inject a) where
  depend a = Dependencies (typeRep a) [] (toDyn (unInject :: Inject a -> a))

instance (Typeable i, Dependable o) => Dependable (i -> o) where
  depend f = inner {dependenciesDependencies = typeRep (Flip f) : dependenciesDependencies inner} where
    inner = (depend $ f undefined)

newtype Flip a b c = Flip (a c b)

use :: Dependable a => a -> Module
use factory = Module $ M.singleton dependenciesTarget [Injection (toDyn factory) dependenciesDependencies dependenciesFinalise] where
  Dependencies{..} = depend factory


newtype Module = Module (M.Map TypeRep [Injection])

data Injection = Injection
  { injectionBase :: Dynamic
  , injectionDependencies :: [TypeRep]
  , injectionFinalise :: Dynamic
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
  ret = fmap unDynamic (dependency $ typeRep ret) where
    unDynamic :: Dynamic -> a
    unDynamic dyn = fromDyn dyn (error (unwords ["BUG in injectedWhy: wrong return type", showDynType dyn]))
  dependency :: TypeRep -> Either [DependencyError] Dynamic
  dependency rep = case fromMaybe [] (M.lookup rep moduleMap) of
    []              -> Left [MissingDependency rep]
    [Injection{..}] -> fmap (apply injectionFinalise)
                       $ foldr cons (Right injectionBase) (reverse injectionDependencies)
    _               -> Left [DuplicateDependency rep]
  cons :: TypeRep -> Either [DependencyError] Dynamic -> Either [DependencyError] Dynamic
  cons xRep fDyn = apply <$> fDyn <+> dependency xRep
  apply :: Dynamic -> Dynamic -> Dynamic
  apply fDyn xDyn
      = fromMaybe
          (error $ unwords
            [ "BUG in injectedWhy: incompatible types when applying function;"
            , showDynType fDyn
            , showDynType xDyn
            ]
          )
          (dynApply fDyn xDyn)

infixl 4 <+> -- same as <*>
(<+>) :: Monoid e => Either e (a -> b) -> Either e a -> Either e b
Left e <+> Left e' = Left (e <> e')
Left e <+> Right _ = Left e
Right _ <+> Left e' = Left e'
Right f <+> Right x = Right (f x)

showDynType :: Dynamic -> String
showDynType dyn = "(" ++ (show . dynTypeRep) dyn ++ ")"
