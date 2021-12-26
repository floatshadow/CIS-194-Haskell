toDigit :: Integer -> [Integer]
toDigit n
  | n <= 0 = []
  | otherwise = toDigit (n `div` 10) ++ [n `mod` 10] 

toDigitRev :: Integer -> [Integer]
toDigitRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = (x*2):[]
doubleEveryOther (x:y:rl) = (x*2):y:doubleEveryOther rl

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [0] = 0 
sumDigits [1] = 1
sumDigits [2] = 2
sumDigits [3] = 3
sumDigits [4] = 4
sumDigits [5] = 5
sumDigits [6] = 6
sumDigits [7] = 7
sumDigits [8] = 8
sumDigits [9] = 9
sumDigits (x:y)  = (sumDigits (toDigit x)) + sumDigits y


validate :: Integer -> Bool
validate n
  | sumDigits (doubleEveryOther (toDigit n)) `mod` 10 == 0   = True
  | otherwise = False



