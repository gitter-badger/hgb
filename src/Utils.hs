module Utils where

isAnyOf :: [c -> Bool] -> c -> Bool
isAnyOf p c = any ($c) p

splitWhen :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = [[]]
splitWhen c xs =
  case remainder of
    (y:ys) -> (first : splitWhen c ys)
    [] -> [first]
  where
    (first, remainder) = break c xs
