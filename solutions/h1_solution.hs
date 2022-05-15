-- Question 1
seqMultiple :: Int -> Int -> [Bool]
seqMultiple n m = [mod i m == 0 | i <- [1..n]]

-- Question 2
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Question 3
listReverse :: [a] -> [a]
listReverse [] = []
listReverse (x:xs) = listReverse xs ++ [x]

-- Question 4
listAdd :: [Int] -> [Int] -> [Int]
listAdd xs [] = xs
listAdd [] ys = ys
listAdd (x:xs) (y:ys) = (x + y) : listAdd xs ys

-- Question 5
inList :: Eq a => [a] -> a -> Bool
inList [] _ = False
inList (x:xs) y
  | x == y    = True
  | otherwise = inList xs y