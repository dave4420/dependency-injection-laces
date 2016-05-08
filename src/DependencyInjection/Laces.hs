module DependencyInjection.Laces
( Inject()
, inject
, unInject
, InjectM()
, injectM
, unInjectM
, Dependable()
, use
, Module()
, ModuleM()
, DependencyError(..)
, componentOrThrow
, componentMay
, componentWhy
, componentOrThrowM
, componentMayM
, componentWhyM
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

newtype InjectM m a = InjectM (m a)
  deriving Typeable

injectM :: m a -> InjectM m a
injectM = InjectM

unInjectM :: InjectM m a -> m a
unInjectM (InjectM x) = x

data Dependencies = Dependencies
  { dependenciesTarget :: TypeRep
  , dependenciesDependencies :: [TypeRep]
  , dependenciesFinalise :: Dynamic
  }

class Typeable a => Dependable a where
  depend :: a -> Dependencies

instance Typeable a => Dependable (Inject a) where
  depend a = Dependencies (typeRep a) [] (toDyn (unInject :: Inject a -> a))

instance (Monad m, Typeable m, Typeable a) => Dependable (InjectM m a) where
  depend ma = undefined

instance (Typeable i, Dependable o) => Dependable (i -> o) where
  depend f = inner {dependenciesDependencies = typeRep (Flip f) : dependenciesDependencies inner} where
    inner = (depend $ f undefined)

newtype Flip a b c = Flip (a c b)

use :: Dependable a => a -> Module
use factory = Module $ M.singleton dependenciesTarget [Injection (toDyn factory) dependenciesDependencies dependenciesFinalise] where
  Dependencies{..} = depend factory


newtype ModuleM (m :: * -> *) = Module (M.Map TypeRep [Injection])
type Module = forall m. ModuleM m

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

instance Monoid (ModuleM m) where
  mempty = Module mempty
  Module x `mappend` Module y = Module (M.unionWith (<>) x y)

componentOrThrow :: Typeable a => Module -> a
componentOrThrow = orThrow . componentWhy

componentMay :: Typeable a => Module -> Maybe a
componentMay = hush . componentWhy

componentWhy :: forall a. Typeable a => Module -> Either [DependencyError] a
componentWhy (Module moduleMap) = ret where
  ret :: Either [DependencyError] a
  ret = fmap unDynamic (dependency $ typeRep ret) where
    unDynamic :: Dynamic -> a
    unDynamic dyn = fromDyn dyn (bug ["wrong return type", showDynType dyn])
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
          (bug
            [ "incompatible types when applying function;"
            , showDynType fDyn
            , showDynType xDyn
            ]
          )
          (dynApply fDyn xDyn)

componentOrThrowM :: (Monad m, Typeable a) => ModuleM m -> m a
componentOrThrowM = fmap orThrow . componentWhyM

componentMayM :: (Monad m, Typeable a) => ModuleM m -> m (Maybe a)
componentMayM = fmap hush . componentWhyM

componentWhyM :: forall m a. (Monad m, Typeable a) => ModuleM m -> m (Either [DependencyError] a)
componentWhyM = undefined

infixl 4 <+> -- same as <*>
(<+>) :: Monoid e => Either e (a -> b) -> Either e a -> Either e b
Left e <+> Left e' = Left (e <> e') -- different from
Left e <+> Right _ = Left e
Right _ <+> Left e' = Left e'
Right f <+> Right x = Right (f x)

showDynType :: Dynamic -> String
showDynType dyn = "(" ++ (show . dynTypeRep) dyn ++ ")"

orThrow :: Either [DependencyError] a -> a
orThrow (Left errors)     = throw (DependencyException errors)
orThrow (Right component) = component

hush :: Either e a -> Maybe a
hush (Left _)  = Nothing
hush (Right x) = Just x

bug :: [String] -> a
bug = error . unwords . ("BUG in DependencyInjection.Laces:" :)
