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
import Data.Functor.Identity
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
  , dependenciesFinalise :: ImpureDynamic
  }

class Typeable a => Dependable a where
  depend :: a -> Dependencies

instance Typeable a => Dependable (Inject a) where
  depend a = Dependencies (typeRep a) [] (toPureDynamic (unInject :: Inject a -> a))

instance (Monad m, Typeable m, Typeable a) => Dependable (InjectM m a) where
  depend ma = undefined

instance (Typeable i, Dependable o) => Dependable (i -> o) where
  depend f = inner {dependenciesDependencies = typeRep (Flip f) : dependenciesDependencies inner} where
    inner = (depend $ f undefined)

newtype Flip a b c = Flip (a c b)

use :: Dependable a => a -> Module
use factory = Module $ M.singleton dependenciesTarget [Injection (toPureDynamic factory) dependenciesDependencies dependenciesFinalise] where
  Dependencies{..} = depend factory


newtype ModuleM (m :: * -> *) = Module (M.Map TypeRep [Injection])
type Module = forall m. ModuleM m

data Injection = Injection
  { injectionBase :: ImpureDynamic
  , injectionDependencies :: [TypeRep]
  , injectionFinalise :: ImpureDynamic
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
{-
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
-}

componentOrThrowM :: (Monad m, Typeable m, Typeable a) => ModuleM m -> m a
componentOrThrowM = fmap orThrow . componentWhyM

componentMayM :: (Monad m, Typeable m, Typeable a) => ModuleM m -> m (Maybe a)
componentMayM = fmap hush . componentWhyM

componentWhyM :: (Monad m, Typeable m, Typeable a) => ModuleM m -> m (Either [DependencyError] a)
componentWhyM = sequence . componentWhyF

componentWhyF :: forall m a. (Monad m, Typeable m, Typeable a) => ModuleM m -> Either [DependencyError] (m a)
componentWhyF (Module moduleMap) = ret where
  ret :: Either [DependencyError] (m a)
  ret = fmap unImpureDynamic (dependency $ typeRep $ Compose ret)
  dependency :: TypeRep -> Either [DependencyError] ImpureDynamic
  dependency rep = case fromMaybe [] (M.lookup rep moduleMap) of
    []              -> Left [MissingDependency rep]
    [Injection{..}] -> fmap (apply injectionFinalise)
                       $ foldr cons (Right injectionBase) (reverse injectionDependencies)
    _               -> Left [DuplicateDependency rep]
  cons :: TypeRep -> Either [DependencyError] ImpureDynamic -> Either [DependencyError] ImpureDynamic
  cons xRep fDyn = apply <$> fDyn <+> dependency xRep

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


data ImpureDynamic = ImpureDynamic
  { impureDynamicValue :: Dynamic
  , impureDynamicAp :: ImpureAp
  }

data ImpureAp
  = PureAp -- ^ accompanying value is pure; requires no knowledge of the specific monad we're using
  | ImpureAp -- ^ accompanying value is impure
    { impureApAp :: Dynamic -- ^ ...and this is how to combine it with another impure function/value
    , impureApFmap :: Dynamic -- ^ ...and this is how to combine it with a pure function
    , impureApReverseFmap :: Dynamic -- ^ ...and this is how to combine it with a pure value
    }

toPureDynamic :: Typeable a => a -> ImpureDynamic
toPureDynamic x = ImpureDynamic{..} where
  impureDynamicValue = toDyn x
  impureDynamicAp = PureAp

unImpureDynamic :: (Applicative f, Typeable f, Typeable a) => ImpureDynamic -> f a
unImpureDynamic ImpureDynamic {impureDynamicValue = dyn, impureDynamicAp = PureAp{}}
  = (pure . fromDyn dyn) (bug ["wrong pure return type", showDynType dyn])
unImpureDynamic ImpureDynamic {impureDynamicValue = dyn, impureDynamicAp = ImpureAp{}}
  = fromDyn dyn (bug ["wrong impure return type", showDynType dyn])

apply :: ImpureDynamic -> ImpureDynamic -> ImpureDynamic
apply ImpureDynamic {impureDynamicAp = PureAp, impureDynamicValue = dynF}
      ImpureDynamic {impureDynamicAp = PureAp, impureDynamicValue = dynX}
    = ImpureDynamic{..}
  where
    impureDynamicAp = PureAp
    impureDynamicValue = dynApplyBug ["incompatible types when applying pure function to pure value"] dynF dynX
apply ImpureDynamic {impureDynamicAp = PureAp, impureDynamicValue = dynF}
      ImpureDynamic {impureDynamicAp = impureDynamicAp @ ImpureAp {impureApFmap = dynMeta}, impureDynamicValue = dynX}
    = ImpureDynamic{..}
  where
    impureDynamicValue
      = dynApplyBug ["incompatible type when applying pure function to impure value (2)"]
                    (dynApplyBug ["incompatible type when applying pure function to impure value (1)"] dynMeta dynF)
                    dynX
apply ImpureDynamic {impureDynamicAp = impureDynamicAp @ ImpureAp {impureApReverseFmap = dynMeta}, impureDynamicValue = dynF}
      ImpureDynamic {impureDynamicAp = PureAp, impureDynamicValue = dynX}
    = ImpureDynamic{..}
  where
    impureDynamicValue
      = dynApplyBug ["incompatible type when applying impure function to pure value (2)"]
                    (dynApplyBug ["incompatible type when applying impure function to pure value (1)"] dynMeta dynF)
                    dynX
apply ImpureDynamic {impureDynamicAp = ImpureAp{}, impureDynamicValue = dynF}
      ImpureDynamic {impureDynamicAp = impureDynamicAp @ ImpureAp {impureApAp = dynMeta}, impureDynamicValue = dynX}
    = ImpureDynamic{..}
  where
    impureDynamicValue
      = dynApplyBug ["incompatible type when applying impure function to impure value (2)"]
                    (dynApplyBug ["incompatible type when applying impure function to impure value (1)"] dynMeta dynF)
                    dynX

dynApplyBug :: [String] -> Dynamic -> Dynamic -> Dynamic
dynApplyBug report dynF dynX = fromMaybe (bug $ report ++ [showDynType dynF, showDynType dynX]) (dynApply dynF dynX)


-- only using this for type hackery, not worth pulling in a dependency
newtype Compose f g a = Compose (f (g a))
