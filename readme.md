# circuits-clash

Traced monoidal categories meet Clash synchronous signals.

This library connects the [circuits](https://github.com/tonyday567/circuits) free traced category framework to [Clash](https://clash-lang.org/) hardware description language. The central discovery: **Clash's `mealy` machine is a concrete instance of the traced category `trace` over the cartesian tensor `(,)`, with `register` providing the causal delay.**

## The discovery

Standard Clash `mealy`:

```haskell
mealy clk rst en f init i =
  let (s', o) = unbundle (f <$> s <*> i)
      s       = register clk rst en init s'
  in  o
```

Decomposed as a traced circuit:

```haskell
mealyAsTrace f init = trace $ ClashCircuit $ \clk rst en sigSI ->
  let (sigS, sigI) = unbundle sigSI
      (sigS', sigO) = unbundle (f <$> sigS <*> sigI)
      sigS'' = register clk rst en init sigS'
  in bundle (sigS'', sigO)
```

The `trace` closes the feedback loop. `register` delays the state by one cycle, making the loop causal and synthesizable. Without `register`, the self-reference diverges — exactly the same behaviour as forgetting `register` in raw Clash.

## Modules

- `Circuit.Clash` — `SignalFun` newtype and `Trace` instance for plain `Signal` arrows.
- `Circuit.Clash.MealyTrace` — `ClashCircuit` newtype (carries clock/reset/enable explicitly), `mealyAsTrace`, `mooreAsTrace`.

## Example

See `examples/MealyTraceExample.hs`. Running it:

```
$ cabal run mealy-trace-example
Standard mealy output:
[0,0,4,13,29,54]
mealyAsTrace output:
[0,0,4,13,29,54]
Agree: True
```

## Building

Requires GHC 9.14+ and the local `clash-prelude` from `~/other/clash-compiler/`. The `cabal.project` wires everything together:

```
cabal build
```
