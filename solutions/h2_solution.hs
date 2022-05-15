-- Question 1
sumTailRec :: Num a => [a] -> a
sumTailRec xs = sumTailRecAux xs 0
  where
    sumTailRecAux :: Num a => [a] -> a -> a
    sumTailRecAux [] r = r
    sumTailRecAux (y:ys) r = sumTailRecAux ys (r + y)

-- Question 2
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ v [] = v
myFoldl f v (x:xs) = foldl f (f v x) xs

-- Question 3
myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr _ v [] = v
myFoldr f v (x:xs) = f x (foldr f v xs)

-- Question 4
alternativeMap :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativeMap _ _ [] = []
alternativeMap f _ [x] = [f x]
alternativeMap f g (x:y:xs) = f x : g y : alternativeMap f g xs

-- Question 5
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = foldl (\acc x -> if p x then acc ++ [x] else acc) [] xs

-- Question 6
sumsqeven :: [Int] -> Int
sumsqeven = sum . map (^2) . filter even

