module DependencyInjection.Laces.ImpureDynamic
( ImpureDynamic()
, toPureDynamic
, toImpureDynamic
, unImpureDynamic
, impureDynamicTypeRep
, applyImpureDynamic
, module Data.Typeable
)
where

import Control.Monad (guard)
import Data.Typeable
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)


data ImpureDynamic (m :: * -> *) = ImpureDynamic
  { value :: Any
  , pureType :: TypeRep
  , purity :: Purity
  }

data Purity = Pure | Impure

toPureDynamic :: Typeable a => a -> (forall m. ImpureDynamic m)
toPureDynamic x = ImpureDynamic{..} where
  value = unsafeCoerce x
  pureType = typeOf x
  purity = Pure

toImpureDynamic :: Typeable a => m a -> ImpureDynamic m
toImpureDynamic x = ImpureDynamic{..} where
  value = unsafeCoerce x
  pureType = typeRep x
  purity = Impure

unImpureDynamic :: (Typeable a, Applicative m) => ImpureDynamic m -> Maybe (m a)
unImpureDynamic ImpureDynamic{..} = ret
  where
    ret = do
      guard $ typeRep (Compose ret) == pureType
      return $ case purity of
        Pure -> pure $ unsafeCoerce value
        Impure -> unsafeCoerce value

impureDynamicTypeRep :: ImpureDynamic m -> TypeRep
impureDynamicTypeRep = pureType

applyImpureDynamic :: forall m. Applicative m => ImpureDynamic m -> ImpureDynamic m -> Maybe (ImpureDynamic m)
applyImpureDynamic f x = do
  resultType <- funResultTy (pureType f) (pureType x)
  let (resultPurity, resultValue) = case (purity f, purity x) of
        (Pure, Pure) -> (Pure, unsafeCoerce (value f) (value x))
        (Pure, Impure) -> (Impure, undefined)
        (Impure, Pure) -> (Impure, undefined)
        (Impure, Impure) -> (Impure, undefined)
  return ImpureDynamic {value = resultValue, pureType = resultType, purity = resultPurity}

-- only using this for type hackery, not worth pulling in a dependency
newtype Compose f g a = Compose (f (g a))
