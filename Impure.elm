module Impure (getRandom) where

-- imports are weird for Native modules
-- You import them as you would normal modules
-- but you can't alias them nor expose stuff from them
import Native.Impure 

-- this will be our function which returns a number plus one
getRandom : () -> Float
getRandom = Native.Impure.getRandom
