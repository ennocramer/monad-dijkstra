# monad-dijkstra

A monad transformer for weighted graph searches using Dijkstra's or A*
algorithm.

## The SearchT Monad Transformer

This library implements the `SearchT` monad transformer, in which
monadic computations can be associated with costs and alternative
computational paths can be explored concurrently.  The API is built in
terms of the `Alternative` and `MonadPlus` type classes and a `cost`
function.

The `runSearchT` function concurrently and lazily evaluates all
alternative computations, returning non-empty solutions in order of
increasing cost.  When forcing only the head of the result list, the
monad performs the minimal amount of work necessary to guarantee the
optimal solution, i.e. with the least cost, is returned first.

## Computational Cost

The cost of a computation is set using the `cost` function. Repeated
calls within a branch of computation will accumulate the cost using
`mappend` from the type's `Monoid` instance. In addition to the
computational cost expended, the `cost` function also accepts a cost
estimate for the rest of computation. Subsequent calls to `cost` will
reset this estimate.

## Limitations

Any type that satisfies the `Monoid` and `Ord` type classes may be
used as a cost values.  However, the internal evaluation algorithm
requires that costs not be negative. That is, for any costs `a` and
`b`, `a <> b >= a && a <> b >= b`.

For the `runSearchT` function to generate solutions in the correct
order, estimates must never overestimate the cost of a computation. If
the cost of a branch is over-estimated or a negative cost is applied,
`runSearchT` may return results in the wrong order.

## Usage Example

```haskell
type Location = ...
type Distance = ...

distance :: Location -> Location -> Distance
distance = ...

neighbors :: Location -> [(Location, Distance)]
neighbors = ...

shortedtRoute :: Location -> Location -> Maybe (Distance, [Location])
shortestRoute from to = listToMaybe $ runSearch $ go [from]
  where
    go ls = if head ls == to
               then return ls
               else msum $ flip map (neighbors (head ls)) $
                   \(l, d) -> cost d (distance l to) $ go (l : ls)
```
