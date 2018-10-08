

-- let cmpt = (\(x,y)->(x,y,compare x y)).(\(x,y,z)->(compare x y, compare x z)).liftM3(,,)intPosMapDepth(intPosMapDepth.stepIntPosMap 0)(intPosMapDepth.stepIntPosMap 1).fromList.b2

-- intPosMapDepth xs <= intPosMapDepth (stepIntPosMap 0 xs) <= intPosMapDepth xs + 1
-- intPosMapDepth xs <= intPosMapDepth (stepIntPosMap 1 xs) <= intPosMapDepth xs + 1

-- This means that the series:

-- intPosMapDepth [x0]
-- intPosMapDepth [x0,x1]
-- intPosMapDepth [x0,x1,x2]
-- intPosMapDepth [x0,x1,x2,x3]
-- intPosMapDepth [x0,x1,x2,x3,..]

-- Are monotonically increasing!

-- So, any depth of a prefix of an array is a minbound of the depth:
--   depth xs <= depth (xs ++ ys) <= depth xs + length ys

-- Cor.:
--   depth (init xs) <= depth xs <= depth (init xs) + 1
--   depth (dropFromEnd n xs) <= depth xs <= depth (dropFromEnd n xs) + n

-- This means that any (n' <= n) length list that has a depth above the one we want
-- is a forbidden prefix.
--   When traversing the tree, we get monotonically increasing along paths and
--   any time it exceeds the max all subtrees can be dropped.


-- Q:
--   How hard is it to count the number of max-depth preserving extensions to a list there are?
--   If it's not too bad, could count arrays of smaller length that reach max depth at their end element.
--     Then count the number of ways that it can be extended without increasing the depths.

-- Note:
--   Probably hard, since we can reach max-depth with a single elements index differences,
--   then have to count up to the max for all of the others..

-- Given a number of elements in the `IntPosMap` already, the max number of distinct elements, and the current depth,
--   count the number of `IntPosMap`'s whose depth does/does not increase by one when a particular element is added.

-- Question:
--   Is is a bijection (toList :: [Int] -> IntPosMap)?
--   Yup.


-- Given a particular element, what next indices do/don't preserve it's depth?

-- Depth 0: ix | [ix, ix + diff, ..]
--   If only one occurrence, any ix preserves its depth.
--   If multiple occurences, only lastIx + diff preserves its depth.

-- Depth 1:
--     Two or more occurences:
--       ix, ix + diff0, ix + diff0 + diff1 (diff0 < diff1)
--       -> [diff0, diff1]
--     diff0 (0..)

--   lastIx + (diff0 | diff1)?


-- Depth 0: ix | [ix..lastIx]
--   Only (lastIx + 1) preserves its depth.

-- Depth 1:
--   [ix..ix'] ++ [ix' + diff0] (0 < diff0)
--     -> {0 => numreps, diff0 => 1 rep}
--   any new 0 would increase depth. (would result in a non-zero difference between indices of the 0)
--   any new diff0 would not increase depth.
--   any other diff would increase depth

--   Increases:
--     lastIx + i | i /= diff0

--   Does not increase:
--     lastIx + diff0 + 1

-- Depth 2:
--   I'm hoping only the last diff of the last diff or something will not increase depth.


-- Want only depth-increasing steps: (xs, x) :: ([Int], Int), i.e. (xs++[x]) has (depth + 1)


