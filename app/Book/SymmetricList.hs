{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Book.SymmetricList where

{-
Symmetric lists tries to improve performances of the normal lists by representing them as a tuple of two lists. The two lists are two parts of a list divided in the middle. The first part of the tuple is the first half of the list and second part is the second half of the same list but reversed
-}
type SymList a = ([a], [a])

{-abstraction function-}
fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys

consSL :: a -> SymList a -> SymList a
consSL a (xs, ys) = if null ys then ([a], xs) else (a: xs, ys)
-- consSL a ([], []) = ([a], [])
-- consSL a (xs, []) = (as, reverse bs) where xss = a : xs; (as, bs) = splitAt (length xs `div` 2) xss
-- consSL a (xs, ys) = (a : xs, ys)

snocSL :: a -> SymList a -> SymList a
snocSL a (xs, ys) = if null xs then (ys, [a]) else (xs, a : ys)

headSL :: SymList a -> Maybe a
headSL ([], ys) = if null ys then Nothing else Just $ head ys
headSL (x : xs, _) = Just x

lastSL :: SymList a -> Maybe a
lastSL (xs, []) = if null xs then Nothing else Just $ head xs
lastSL (_, ys) = if null ys then Nothing else Just $ head ys

tailSL :: SymList a -> Maybe ( SymList a )
tailSL ([], []) = Nothing
tailSL ([], [x]) = Just ([], [])
tailSL ([x], ys) = Just (reverse as, bs) where (as, bs) = splitAt (length ys `div` 2) ys
tailSL (xs, ys) = Just (tail xs, ys)

initSL :: SymList a -> Maybe (SymList a)
initSL ([], []) = Nothing
initSL ([], [x]) = Just ([], [])
initSL (xs, [y]) = Just (as, reverse bs) where (as, bs) = splitAt (length xs `div` 2) xs
initSL (xs, ys) = Just (xs, tail ys)

nilSL :: SymList a
nilSL = ([], [])

nullSL :: SymList a -> Bool
nullSL ([], []) = True
nullSL _ = False

lengthSL :: SymList a -> Int
lengthSL (xs, ys) = length xs + length ys

singleSL :: SymList a -> Bool
singleSL ([a], _) = True
singleSL (_, [a]) = True
singleSL (_, _) = False
