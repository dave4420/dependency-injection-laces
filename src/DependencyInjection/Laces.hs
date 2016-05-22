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
import Data.Functor.Identity
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import GHC.Exts (Constraint)

import DependencyInjection.Laces.ImpureDynamic

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

data Dependencies m = Dependencies
  { dependenciesTarget :: TypeRep
  , dependenciesDependencies :: [TypeRep]
  , dependenciesFinalise :: ImpureDynamic m
  }

class Typeable a => Dependable a where
  type Context a (m :: * -> *) :: Constraint
  depend :: a -> (forall m. Context a m => Dependencies m)

instance Typeable a => Dependable (Inject a) where
  type Context (Inject a) m = ()
  depend a = Dependencies (typeRep a) [] (toPureDynamic (unInject :: Inject a -> a))

instance (Monad m, Typeable m, Typeable a) => Dependable (InjectM m a) where
  type Context (InjectM m a) m' = m ~ m'
  depend ma = undefined

instance (Typeable i, Dependable o) => Dependable (i -> o) where
  type Context (i -> o) m = Context o m
  depend f = inner {dependenciesDependencies = typeRep (Flip f) : dependenciesDependencies inner} where
    inner = (depend $ f undefined)

newtype Flip a b c = Flip (a c b)

use :: Dependable a => a -> (forall m. Context a m => ModuleM m)
use factory = Module $ M.singleton dependenciesTarget [Injection (toPureDynamic factory) dependenciesDependencies dependenciesFinalise] where
  Dependencies{..} = depend factory


newtype ModuleM (m :: * -> *) = Module (M.Map TypeRep [Injection m])
type Module = forall m. ModuleM m

data Injection m = Injection
  { injectionBase :: ImpureDynamic m
  , injectionDependencies :: [TypeRep]
  , injectionFinalise :: ImpureDynamic m
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

componentWhy :: {-forall a.-} Typeable a => Module -> Either [DependencyError] a
componentWhy module' = (runIdentity . componentWhyM) module' -- pointfree version doesn't compile

componentOrThrowM :: (Monad m, Typeable m, Typeable a) => ModuleM m -> m a
componentOrThrowM = fmap orThrow . componentWhyM

componentMayM :: (Monad m, Typeable m, Typeable a) => ModuleM m -> m (Maybe a)
componentMayM = fmap hush . componentWhyM

componentWhyM :: (Monad m, Typeable m, Typeable a) => ModuleM m -> m (Either [DependencyError] a)
componentWhyM = sequence . componentWhyF

componentWhyF :: forall m a. (Monad m, Typeable m, Typeable a) => ModuleM m -> Either [DependencyError] (m a)
componentWhyF (Module moduleMap) = ret where
  ret :: Either [DependencyError] (m a)
  ret = fmap unImpureDynamicBug (dependency $ typeRep $ Compose ret)
  dependency :: TypeRep -> Either [DependencyError] (ImpureDynamic m)
  dependency rep = case fromMaybe [] (M.lookup rep moduleMap) of
    []              -> Left [MissingDependency rep]
    [Injection{..}] -> fmap (applyBug injectionFinalise)
                       $ foldr cons (Right injectionBase) (reverse injectionDependencies)
    _               -> Left [DuplicateDependency rep]
  cons :: TypeRep -> Either [DependencyError] (ImpureDynamic m) -> Either [DependencyError] (ImpureDynamic m)
  cons xRep fDyn = applyBug <$> fDyn <+> dependency xRep

infixl 4 <+> -- same as <*>
(<+>) :: Monoid e => Either e (a -> b) -> Either e a -> Either e b
Left e <+> Left e' = Left (e <> e') -- different from
Left e <+> Right _ = Left e
Right _ <+> Left e' = Left e'
Right f <+> Right x = Right (f x)

showDynType :: ImpureDynamic m -> String
showDynType dyn = "(" ++ (show . impureDynamicTypeRep) dyn ++ ")"

orThrow :: Either [DependencyError] a -> a
orThrow (Left errors)     = throw (DependencyException errors)
orThrow (Right component) = component

hush :: Either e a -> Maybe a
hush (Left _)  = Nothing
hush (Right x) = Just x

bug :: [String] -> a
bug = error . unwords . ("BUG in DependencyInjection.Laces:" :)


applyBug :: Applicative m => ImpureDynamic m -> ImpureDynamic m -> ImpureDynamic m
applyBug dynF dynX
  = fromMaybe (bug ["incompatible types when applying function:", showDynType dynF, showDynType dynX])
              (applyImpureDynamic dynF dynX)

unImpureDynamicBug :: (Typeable a, Applicative m) => ImpureDynamic m -> m a
unImpureDynamicBug dyn
  = fromMaybe (bug ["unexpected type when extracting value", showDynType dyn]) (unImpureDynamic dyn)

-- only using this for type hackery, not worth pulling in a dependency
newtype Compose f g a = Compose (f (g a))
