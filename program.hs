factorial :: Integer -> Integer

factorial 0 = 1 -- Base Case
factorial n = n * factorial (n-1)

double :: Integer -> Integer
double x = 2 * x

absValue :: Integer -> Integer
absValue n
    | n < 0 = n * (-1) -- -n
    | otherwise = n

power :: Integer -> Integer -> Integer
power _ 0 = 1 -- Base case
power x p
    | even p = n * n
    | otherwise = n * n * x
    where
        n = power x (p `div` 2)

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False

isPrime x = not (hasDivisor(x - 1))
    where
        hasDivisor:: Int -> Bool
        hasDivisor 1 = False
        hasDivisor n = mod x n == 0 || hasDivisor(n - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci(x-1) + fibonacci(x - 2)
