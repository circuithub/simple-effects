# 0.13.0.0
* Support for unique parametrized effects.
    * Each effect can provide an extra constrain on the monad in which it executes
    * One of the applications is a class that provides an extra functional dependency on some of the effects parameters
* State effect now has a functional dependency on the parameter meaning you can only have one state effect in a computation, but the inferrence is much better
* Module for working with newtypes of effects
    * Allows you to tag a certain effect with a new name so multiples of the same effect can be used without interaction
    * This lets you recover the ability to use multiple state effects in the same computation
* Yield effect also gets a functional dependency
* Async effect gets a parameter with a functional dependency enabling different implementations to use different thread representations
* Removed Parallel module. Pretty much covered by Async
* Ability to cancel thread and check if they're done with Async
* Better errors when failing to derive an Effect instance
* Better docs
