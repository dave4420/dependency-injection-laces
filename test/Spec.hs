import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

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
