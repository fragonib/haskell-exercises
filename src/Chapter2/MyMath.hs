module Chapter2.MyMath where

unzip :: [(Int, Int)] -> ([Int], [Int])
unzip [] = ([], [])
unzip ((first, second):xs) =
  (first : restFirsts, second : restSeconds)
  where (restFirsts, restSeconds) = Chapter2.MyMath.unzip xs
  
ackermann :: (Int, Int) -> Int
ackermann (m, n) 
  | m==0 = n+1
  | m>0, n==0 = ackermann (m-1, 1)
  | m>0, n>0 = ackermann (m-1, ackermann(m, n-1))
