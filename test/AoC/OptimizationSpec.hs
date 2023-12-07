{-# LANGUAGE ScopedTypeVariables #-}

module AoC.OptimizationSpec where

import AoC.Optimization (hillClimbWithStep)
import Data.Foldable (traverse_)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "optimization" $ do
  describe "hill climbing" $ do
    it "returns the initial value for a perfectly flat function" $
      hillClimbWithStep (\(_ :: Int) -> 4) 19 100 `shouldBe` (19, 4)
    it "maxes a simple quadratic under different conditions" $ do
      let simpleQuadratic (x :: Int) = x * (7 - x)
          maxes = [(3, 12), (4, 12)]
       in traverse_
            (`shouldSatisfy` (`elem` maxes))
            [ hillClimbWithStep simpleQuadratic 0 4,
              hillClimbWithStep simpleQuadratic 0 100,
              hillClimbWithStep simpleQuadratic (-30) 100,
              hillClimbWithStep simpleQuadratic (-30) 4,
              hillClimbWithStep simpleQuadratic 90 4,
              hillClimbWithStep simpleQuadratic 90 100
            ]
    it "maxes a bigger quadratic under different conditions" $
      let biggerQuadratic (x :: Int) = (21 + x) * (83 - x)
          solution = (31, 2704)
       in traverse_
            (`shouldBe` solution)
            [ hillClimbWithStep biggerQuadratic 10000 200,
              hillClimbWithStep biggerQuadratic 0 200,
              hillClimbWithStep biggerQuadratic (-10000) 200,
              hillClimbWithStep biggerQuadratic 10000 5,
              hillClimbWithStep biggerQuadratic 0 5,
              hillClimbWithStep biggerQuadratic (-10000) 5
            ]
