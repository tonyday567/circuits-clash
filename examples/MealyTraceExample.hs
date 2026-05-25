{-# LANGUAGE DataKinds #-}

-- | Demonstration: Mealy machines are traced circuits.
--
-- This example shows that Clash's standard @mealy@ and our
-- 'mealyAsTrace' produce identical output for a multiply-accumulate
-- (MAC) circuit.
module Main where

import Circuit.Clash.MealyTrace
import Clash.Explicit.Mealy qualified as Clash
import Clash.Explicit.Prelude hiding ((++))

-- | Multiply-accumulate transfer function.
macT :: Int -> (Int, Int) -> (Int, Int)
macT s (x, y) = (s', s)
  where
    s' = x * y + s

-- | Standard Clash mealy machine.
macStandard ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Int, Int) ->
  Signal System Int
macStandard clk rst en = Clash.mealy clk rst en macT 0

-- | The same machine, expressed as a traced circuit.
macAsTrace ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Int, Int) ->
  Signal System Int
macAsTrace clk rst en =
  runClashCircuit (mealyAsTrace macT 0) clk rst en

-- | Run both and show they agree.
main :: IO ()
main = do
  let inputs = (1, 1) : (2, 2) : (3, 3) : (4, 4) : (5, 5) : [(0, 0) | _ <- [1 ..]]
      clk = systemClockGen
      rst = systemResetGen
      en = enableGen
      outStandard = sampleN 6 (macStandard clk rst en (fromList inputs))
      outTrace = sampleN 6 (macAsTrace clk rst en (fromList inputs))
  putStrLn "Standard mealy output:"
  print outStandard
  putStrLn "mealyAsTrace output:"
  print outTrace
  putStrLn $ "Agree: " ++ show (outStandard == outTrace)
