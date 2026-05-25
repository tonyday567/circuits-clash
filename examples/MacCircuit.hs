{-# LANGUAGE DataKinds #-}

-- | Demonstration: building a circuit from the free GADT and reifying it
-- to a ClashCircuit.
--
-- This example shows the full pipeline:
--
--   1. Define a transfer function with internal register delay as a ClashCircuit
--   2. Close the feedback loop with @Knot@
--   3. @reify@ the free Circuit to a concrete ClashCircuit
--   4. Run on signals — identical to standard Clash mealy
--
-- The composition is deliberately simple: one @Knot@, one @reify@.  This
-- shows that @Knot@ (the free traced monoidal category's feedback
-- constructor) and @reify@ (the interpretation via the @Trace@ instance)
-- compile to the same lazy-knot pattern that standard @mealy@ uses.
module Main where

import Circuit (Circuit (Knot), reify)
import Circuit.Clash.MealyTrace (ClashCircuit (..))
import Clash.Explicit.Mealy qualified as Clash
import Clash.Explicit.Prelude hiding ((++))

-- | Multiply-accumulate transfer function.
macT :: Int -> (Int, Int) -> (Int, Int)
macT s (x, y) = (s', s)
  where
    s' = x * y + s

-- | The MAC transfer function with internal register delay, as a
-- ClashCircuit.
--
-- Takes @(state, input)@ and produces @(nextState, output)@, with the
-- next state delayed by one cycle through @register@.  The register
-- makes the feedback loop causal — without it, @reify@ would diverge.
macStage :: ClashCircuit System (Int, (Int, Int)) (Int, Int)
macStage = ClashCircuit $ \clk rst en sigSI -> do
  let (sigS, sigI) = unbundle sigSI
      sigSO = macT <$> sigS <*> sigI
      (sigS', sigO) = unbundle sigSO
      sigS'' = register clk rst en 0 sigS'
  bundle (sigS'', sigO)

-- | Standard Clash mealy machine for comparison.
macStandard ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Int, Int) ->
  Signal System Int
macStandard clk rst en = Clash.mealy clk rst en macT 0

-- | The MAC circuit, built from the free GADT.
--
-- @Knot@ closes the feedback loop over the cartesian tensor @(,)@:
-- the state output of @macStage@ is fed back as the state input.
-- @Lift@ embeds the concrete @ClashCircuit@ arrow.
--
-- @reify macCircuit@ uses the @Trace (ClashCircuit dom) (,)@ instance
-- to tie the lazy knot over @Signal@, producing a single
-- @ClashCircuit dom (Int, Int) Int@.
macCircuit :: Circuit (ClashCircuit System) (,) (Int, Int) Int
macCircuit = Knot macStage

-- | The reified circuit, ready to run on signals.
macAsCircuit ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Int, Int) ->
  Signal System Int
macAsCircuit clk rst en =
  runClashCircuit (reify macCircuit) clk rst en

-- | Run standard mealy and the reified circuit; show they agree.
main :: IO ()
main = do
  let inputs =
        (1, 1) : (2, 2) : (3, 3) : (4, 4) : (5, 5) : [(0, 0) | _ <- [1 ..]]
      clk = systemClockGen
      rst = systemResetGen
      en = enableGen
      outStandard = sampleN 6 (macStandard clk rst en (fromList inputs))
      outCircuit = sampleN 6 (macAsCircuit clk rst en (fromList inputs))
  putStrLn "Standard mealy output:"
  print outStandard
  putStrLn "reify (Knot macStage):"
  print outCircuit
  putStrLn $ "Agree: " ++ show (outStandard == outCircuit)
