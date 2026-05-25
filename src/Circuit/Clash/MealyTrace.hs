-- | Mealy machines as traced circuits.
--
-- This module contains the central discovery: Clash's @mealy@ is a
-- concrete instance of the traced monoidal category 'trace' over the
-- cartesian tensor @(,)@, with 'register' providing the causal delay
-- that makes the feedback loop synthesizable.
--
-- The decomposition:
--
-- @
-- mealy f init = trace (register init . f')
-- @
--
-- where @f'@ splits the input signal, applies the transfer function,
-- and pairs the next state with the output. The 'register' delays
-- the state by one cycle, breaking the circular dependency.
--
-- We provide two arrow types:
--
--   * 'ClashCircuit' — carries clock, reset, and enable explicitly.
--     This is the direct Clash style; 'mealyAsTrace' uses it.
--
--   * 'SignalFun' (from "Circuit.Clash") — the plain signal arrow.
--     'mealyAsTraceSignal' shows the same decomposition without the
--     explicit clock/reset/enable plumbing.
module Circuit.Clash.MealyTrace
  ( -- * ClashCircuit arrow
    ClashCircuit (..),

    -- * Mealy as trace
    mealyAsTrace,
    mooreAsTrace,
    mealyAsTraceSignal,
    mooreAsTraceSignal,

    -- * Re-export for convenience
    SignalFun (..),
  )
where

import Circuit (Trace (..))
import Circuit.Clash (SignalFun (..))
import Clash.Explicit.Prelude
  ( Bundle (..),
    Clock,
    Enable,
    KnownDomain,
    NFDataX,
    Reset,
    Signal,
    register,
  )
import Control.Category (Category (..))
import Prelude hiding (id, (.))

-- * ClashCircuit — signal arrow with explicit clock/reset/enable

-- | A signal function that carries its own clock, reset, and enable.
--
-- This is the natural arrow for hardware designs in explicit Clash
-- style. Composition threads the same clock/reset/enable through
-- both stages. The 'Trace' instance closes feedback loops over
-- 'Signal', with the same laziness semantics as the pure @(->)@
-- instance.
newtype ClashCircuit dom a b = ClashCircuit
  { runClashCircuit ::
      Clock dom ->
      Reset dom ->
      Enable dom ->
      Signal dom a ->
      Signal dom b
  }

instance Category (ClashCircuit dom) where
  id = ClashCircuit $ \_ _ _ -> id
  ClashCircuit f . ClashCircuit g =
    ClashCircuit $ \clk rst en ->
      f clk rst en . g clk rst en

-- | Traced instance for explicit clock/reset/enable circuits.
--
-- The trace ties a lazy knot. For synthesis, the inner morphism
-- must contain a 'register' (or 'delay') on the feedback wire.
instance Trace (ClashCircuit dom) (,) where
  trace (ClashCircuit f) =
    ClashCircuit $ \clk rst en sigB ->
      let sigAC = f clk rst en (bundle (sigA, sigB))
          (sigA, sigC) = unbundle sigAC
       in sigC

  untrace (ClashCircuit f) =
    ClashCircuit $ \clk rst en sigAB ->
      let (sigA, sigB) = unbundle sigAB
       in bundle (sigA, f clk rst en sigB)

-- * Mealy and Moore as traced circuits

-- | Mealy machine as a traced circuit.
--
-- This is the discovery: 'mealy' decomposes into 'trace' plus
-- 'register'. The transfer function @f :: s -> i -> (s, o)@ is
-- lifted to signals, the next state is delayed by one cycle, and
-- the loop is closed by 'trace'.
--
-- Compare with the standard Clash definition:
--
-- @
-- mealy clk rst en f init i =
--   let (s', o) = unbundle (f <$> s <*> i)
--       s       = register clk rst en init s'
--   in  o
-- @
--
-- Here the same pattern is expressed compositionally:
--
-- @
-- mealyAsTrace f init = trace (register init . liftTransfer f)
-- @
mealyAsTrace ::
  (KnownDomain dom, NFDataX s) =>
  (s -> i -> (s, o)) ->
  s ->
  ClashCircuit dom i o
mealyAsTrace f iS =
  trace $
    ClashCircuit $ \clk rst en sigSI ->
      let (sigS, sigI) = unbundle sigSI
          sigSO = f <$> sigS <*> sigI
          (sigS', sigO) = unbundle sigSO
          sigS'' = register clk rst en iS sigS'
       in bundle (sigS'', sigO)

-- | Moore machine as a traced circuit.
--
-- A Moore machine separates the state transition @ft :: s -> i -> s@
-- from the output projection @fo :: s -> o@. It is also a trace:
-- the state transition is looped through 'register', and the output
-- is projected from the current state.
mooreAsTrace ::
  (KnownDomain dom, NFDataX s) =>
  (s -> i -> s) ->
  (s -> o) ->
  s ->
  ClashCircuit dom i o
mooreAsTrace ft fo iS =
  trace $
    ClashCircuit $ \clk rst en sigSI ->
      let (sigS, sigI) = unbundle sigSI
          sigS' = ft <$> sigS <*> sigI
          sigS'' = register clk rst en iS sigS'
          sigO = fo <$> sigS''
       in bundle (sigS'', sigO)

-- * Plain signal variants (no explicit clock/reset/enable)

-- | 'mealyAsTrace' for the plain 'SignalFun' arrow.
--
-- The caller must provide a delayed state signal; 'register' is
-- applied inside the trace body. This variant is useful when
-- clock/reset/enable are in scope implicitly or not needed.
mealyAsTraceSignal ::
  (KnownDomain dom, NFDataX s) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  s ->
  (s -> i -> (s, o)) ->
  SignalFun dom i o
mealyAsTraceSignal clk rst en iS f =
  trace $
    SignalFun $ \sigSI ->
      let (sigS, sigI) = unbundle sigSI
          sigSO = f <$> sigS <*> sigI
          (sigS', sigO) = unbundle sigSO
          sigS'' = register clk rst en iS sigS'
       in bundle (sigS'', sigO)

-- | 'mooreAsTrace' for the plain 'SignalFun' arrow.
mooreAsTraceSignal ::
  (KnownDomain dom, NFDataX s) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  s ->
  (s -> i -> s) ->
  (s -> o) ->
  SignalFun dom i o
mooreAsTraceSignal clk rst en iS ft fo =
  trace $
    SignalFun $ \sigSI ->
      let (sigS, sigI) = unbundle sigSI
          sigS' = ft <$> sigS <*> sigI
          sigS'' = register clk rst en iS sigS'
          sigO = fo <$> sigS''
       in bundle (sigS'', sigO)
