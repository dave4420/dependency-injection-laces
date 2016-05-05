# A (Toy) Dependency Injection Framework For Haskell

## Done

 *  some way of wiring up dependencies

## To Do

 *  documentation
 *  support monadic dependencies (e.g. a component that requires an IORef)
 *  upload to Hackage
 *  some way of solving the robot legs problem
 *  support polymorphic factories (i.e. the component type is parameterised by a monad, as are its dependencies;
    we shouldn't have to specify which monad when registering the factory in a module, only when we ask the module
    for a component) (this might not be possible)
