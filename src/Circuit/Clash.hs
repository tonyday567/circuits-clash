-- | Clash as a traced monoidal category.
--
-- This module provides the foundational 'SignalFun' newtype and its
-- 'Trace' instance over the cartesian tensor @(,)@. The instance is
-- the direct lazy-knot translation from 'Circuit.Trace' to Clash's
-- 'Signal' type — feedback is closed by self-referential binding,
-- exactly as in plain Haskell functions.
--
-- For causal loops (the kind that synthesize to hardware), the
-- feedback path must contain a delay element such as 'register' or
-- 'delay'. Without one, the knot diverges at simulation time and
-- fails synthesis — the same behaviour as a raw Clash feedback loop.
--
-- For a version that carries clock, reset, and enable explicitly,
-- see 'Circuit.Clash.MealyTrace'.
module Circuit.Clash
  ( -- * Signal arrow
    SignalFun (..),

    -- * Re-exports from circuits
    Circuit (..),
    Wire,
    Step,
    reify,
    Trace (..),
    Hyper (..),
    run,
    lift,
    lower,
    encode,
  )
where

import Circuit
  ( Circuit (..),
    Hyper (..),
    Step,
    Trace (..),
    Wire,
    encode,
    lift,
    lower,
    reify,
    run,
  )
import Clash.Explicit.Prelude
  ( Bundle (..),
    Signal,
  )
import Control.Category (Category (..))
import Prelude hiding (id, (.))

-- | A function between synchronous signals.
--
-- @SignalFun dom a b@ is the category of Clash signals on domain
-- @dom@, with objects as types and morphisms as @Signal -> Signal@
-- functions. It is the natural arrow type for lifting circuits into
-- the Clash simulation/synthesis world.
newtype SignalFun dom a b = SignalFun
  { runSignalFun :: Signal dom a -> Signal dom b
  }

instance Category (SignalFun dom) where
  id = SignalFun id
  SignalFun f . SignalFun g = SignalFun (f . g)

-- | Traced monoidal category instance for synchronous signals.
--
-- The cartesian trace ties a lazy knot over 'Signal'. For a causal
-- loop, the body of the trace must contain a register or delay on
-- the feedback wire; otherwise the self-reference diverges.
--
-- This is the semantic foundation that makes 'mealyAsTrace' possible:
-- a Mealy machine is exactly a traced circuit whose feedback path
-- passes through a one-cycle 'register'.
instance Trace (SignalFun dom) (,) where
  trace (SignalFun f) =
    SignalFun $ \sigB ->
      let sigAC = f (bundle (sigA, sigB))
          (sigA, sigC) = unbundle sigAC
       in sigC

  untrace (SignalFun f) =
    SignalFun $ \sigAB ->
      let (sigA, sigB) = unbundle sigAB
       in bundle (sigA, f sigB)
