# find-cycles

Find cycles efficiently by building up a recursive map of element positions.

## QA

- How efficiently?
  + I'm not sure. There are some benchmarks, but I haven't yet proven good bounds.

- What's good about this method?
  + It has fast updates, which makes it suitable for usage with streamed input.
    For example, I often have a lazy infinite list that represents an OEIS
    series. I'd like to find cycles in the original series, `n`-th differences,
    etc. without adding too much overhead to calculating the series.
  + If the input set is small enough, it's memory efficient
  + If we modify the data types slightly, we should be able to drop positions
    outside a finite window lazily. This should give us a `O(1)` (after
    the window size, input set size are determined) algorithm to update
    the position map with a new element.

- What's left to do?
  + A good bit of maximal size analysis
    * Worst case isn't too good, but average case appears to be quite efficient
  + Finish using `FreeSemigroup`s to extract eventual cycles from position maps
  + A decent bit of testing and benchmarking


### Data.BStream

```haskell
-- | Infinite complete binary trees
newtype BStream a = BStream
  { runBStream :: Cofree Pair a
  } deriving (Functor)
```


### Data.Cycle

- Prototypes to represent cycles
  + Optionally with gaps (i.e. blank values)


### Data.Cycle.Find

- Methods to find cycles
- Simple representation of cycles
- Methods to recognize cycles
- `Indexed` values


### Data.FiniteStack

Prototype finite stacks using `MVector`s


### Data.IntPosMap

- `Int` position maps of finite series
- Enumeration of `IntPosMap`s on various classes of series
- Calculation of the distinct depths of an `IntPosMap`s tree
- Size/time analysis
- Pretty printing
- Conversion to (gapped) cycles


### Data.List.Utils

- Tallying
- Binomial transformations
- Maximums with multiplicity
- Timed `IO`
- Misc.


### Data.Map.Utils

- Upsert
- Conversion
- `foldMapWithKey1 :: Semigroup b => (k -> a -> b) -> Map k a -> b`


### Data.Pair

```haskell
-- | A lazy pair of values
newtype Pair a = Pair { getPair :: (a, a) } deriving (Functor)
```


### Data.PosMap

- `PosMap`: A recursive map of the positions of values
- Conversion to `IntPosMap`
- Misc. utilities


### Data.Semigroup.Free

- `Semialternative`:

```haskell
class Applicative f => Semialternative f where
  (<!>) :: f a -> f a -> f a

  saconcat :: NonEmpty (f a) -> f a

  saconcatMap :: (a -> f b) -> NonEmpty a -> f b
```

- `SemigroupFree`:

```haskell
class Semialternative f => SemigroupFree f where
  foldMap1 :: Semigroup s => (a -> s) -> f a -> s

  semigroupFree :: SemigroupFree g => f a -> g a
```

- 8 implementations, with tests and benchmarks:
  + `NonEmpty`
  + `FreeSemigroup`
  + `FreeSemigroup'`
  + `FreePS`
  + `FPS`
  + `FSemigroup`
  + `FS`
  + `FS'`


### Data.Sequence1

A prototype non-empty `Seq`


### Data.Tree.Utils

- Operations with paths
- Filtering subtrees
- Generating `Tree`s of cycles
- Combinations of scanning and filtering `Tree`s
- Methods to draw `Tree`s and `Forest`s in more
  convenient ways.



