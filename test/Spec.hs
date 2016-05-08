import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Control.Monad.Trans.Writer
import Data.Typeable

import DependencyInjection.Laces

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = $(testGroupGenerator)


newtype Foo = Foo Int
  deriving (Eq, Show, Arbitrary)

newtype Bar = Bar Int
  deriving (Eq, Show, Arbitrary)

data Composite = Composite Foo Bar
  deriving (Eq, Show)

composite foo bar = inject $ Composite foo bar

compositeM :: Foo -> Bar -> InjectM (Writer [String]) Composite
compositeM foo bar = injectM $ Composite foo bar <$ tell ["Composite"]

data FooComposite = FooComposite Foo Composite
  deriving (Eq, Show)

fooCompositeM :: Foo -> Composite -> InjectM (Writer [String]) FooComposite
fooCompositeM foo composite = injectM $ FooComposite foo composite <$ tell ["FooComposite"]

prop_retrieveTargetThatHasNoDependencies = do
  givenFoo <- arbitrary :: Gen Foo
  givenBar <- arbitrary :: Gen Bar
  let module'
        = mconcat
          [ use $ inject givenFoo
          , use $ inject givenBar
          , use composite
          ]
  let result = componentWhy module'
  return $ result === Right (givenFoo)

prop_retrieveTargetThatHasDependencies = do
  givenFoo <- arbitrary :: Gen Foo
  givenBar <- arbitrary :: Gen Bar
  let module'
        = mconcat
          [ use $ inject givenFoo
          , use $ inject givenBar
          , use composite
          ]
  let result = componentWhy module'
  return $ result === Right (Composite givenFoo givenBar)

prop_failWhenTargetNotPresent = do
  givenFoo <- arbitrary :: Gen Foo
  givenBar <- arbitrary :: Gen Bar
  let module'
        = mconcat
          [ use $ inject givenBar
          , use composite
          ]
  let result :: Either [DependencyError] Foo
      result = componentWhy module'
  return $ result === Left [MissingDependency (typeOf givenFoo)]

prop_failWhenDependencyNotPresent = do
  givenFoo <- arbitrary :: Gen Foo
  givenBar <- arbitrary :: Gen Bar
  let module'
        = mconcat
          [ use $ inject givenBar
          , use composite
          ]
  let result :: Either [DependencyError] Composite
      result = componentWhy module'
  return $ result === Left [MissingDependency (typeOf givenFoo)]

prop_failWhenDuplicatesPresent = do
  givenFoo <- arbitrary :: Gen Foo
  givenBar <- arbitrary :: Gen Bar
  let module'
        = mconcat
          [ use $ inject givenFoo
          , use $ inject givenFoo -- doesn't matter that they're the same
          , use $ inject givenBar
          , use composite
          ]
  let result :: Either [DependencyError] Composite
      result = componentWhy module'
  return $ result === Left [DuplicateDependency (typeOf givenFoo)]

prop_retrieveMonadicTargetThatHasNoDependencies = do
  givenFoo <- arbitrary :: Gen Foo
  givenBar <- arbitrary :: Gen Bar
  let module'
        = mconcat
          [ (use . injectM) (givenFoo <$ tell ["Foo"] :: Writer [String] Foo)
          , (use . injectM) (givenBar <$ tell ["Bar"] :: Writer [String] Bar)
          , use compositeM
          ]
  let result :: Either [DependencyError] Foo
      (result, calls) = runWriter $ componentWhyM module'
  return $ result === Right (givenFoo) .&&. calls === ["Foo"]

prop_retrieveMonadicTargetThatHasDependencies = do
  givenFoo <- arbitrary :: Gen Foo
  givenBar <- arbitrary :: Gen Bar
  let module'
        = mconcat
          [ (use . injectM) (givenFoo <$ tell ["Foo"] :: Writer [String] Foo)
          , (use . injectM) (givenBar <$ tell ["Bar"] :: Writer [String] Bar)
          , use compositeM
          ]
  let result :: Either [DependencyError] Composite
      (result, calls) = runWriter $ componentWhyM module'
  return $ result === Right (Composite givenFoo givenBar)
            .&&. (calls === ["Foo", "Bar", "Composite"] .||. calls === ["Bar", "Foo", "Composite"])

prop_retrieveMonadicTargetThatIncludesAMonadicDependencyTwiceButExecuteItOnlyOnce = do
  givenFoo <- arbitrary :: Gen Foo
  givenBar <- arbitrary :: Gen Bar
  let module'
        = mconcat
          [ (use . injectM) (givenFoo <$ tell ["Foo"] :: Writer [String] Foo)
          , (use . injectM) (givenBar <$ tell ["Bar"] :: Writer [String] Bar)
          , use compositeM
          , use fooCompositeM
          ]
  let result :: Either [DependencyError] FooComposite
      (result, calls) = runWriter $ componentWhyM module'
  return $ result === Right (FooComposite givenFoo $ Composite givenFoo givenBar)
            .&&. (calls === ["Foo", "Bar", "Composite", "FooComposite"]
                  .||. calls === ["Bar", "Foo", "Composite", "FooComposite"])

prop_retrieveMonadicTargetTwiceButExecuteItOnlyOnce = do
  givenFoo <- arbitrary :: Gen Foo
  givenBar <- arbitrary :: Gen Bar
  let module'
        = mconcat
          [ (use . injectM) (givenFoo <$ tell ["Foo"] :: Writer [String] Foo)
          , (use . injectM) (givenBar <$ tell ["Bar"] :: Writer [String] Bar)
          , use compositeM
          ]
  let resultX, resultY :: Either [DependencyError] Foo
      ((resultX, resultY), calls) = runWriter $ do
        x <- componentWhyM module'
        y <- componentWhyM module'
        return (x, y)
  return $ resultX === Right givenFoo .&&. resultY === Right givenFoo .&&. calls === ["Foo"]
