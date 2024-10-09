import Distribution.Simple.Utils (xargs)

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits x = if x < 0
                then []
                else toDigits (x `div` 10) ++ [x `mod` 10]

intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther xs = zipWith (+) (cycle sol) xs
                      where sol = map fromEnum [even len, odd len] 
                            len = length xs


main = do
    putStrLn (show(doubleEveryOther(toDigits (2222)) ))