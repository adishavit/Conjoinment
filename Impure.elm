module Impure (getRandom) where

-- See: https://github.com/NoRedInk/take-home/wiki/Writing-your-first-Elm-Native-module

-- imports are weird for Native modules
-- You import them as you would normal modules
-- but you can't alias them nor expose stuff from them
import Native.Impure 

-- this will be our function which returns a number plus one
getRandom : () -> Float
getRandom = Native.Impure.getRandom
