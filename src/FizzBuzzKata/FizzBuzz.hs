module FizzBuzzKata.FizzBuzz where


fizzbuzz :: Int -> [Char]
fizzbuzz x
  | (x `mod` 3 == 0) && (x `mod` 5 == 0) = "FizzBuzz"
  | x `mod` 3 == 0 = "Fizz"
  | x `mod` 5 == 0 = "Buzz"
  | otherwise = show x


topFizzBuzz :: Int -> [[Char]]
topFizzBuzz n = map fizzbuzz [1..n]

main :: IO()
main = print $ topFizzBuzz 1000
