-- Prática 03 de Haskell
-- Nome: Bento Borges Schirmer

add10toall :: [Int] -> [Int]
add10toall numbers = [number + 10 | number <- numbers]

multN :: Int -> [Int] -> [Int]
multN n numbers = [number * n | number <- numbers]

multN' :: Int -> [Int] -> [Int]
multN' n = map (*n)

applyExpr :: [Int] -> [Int]
applyExpr xs = [3*x+2 | x <- xs]

applyExpr' :: [Int] -> [Int]
applyExpr' = map (\x -> 3*x+2)

addSuffix :: String -> [String] -> [String]
addSuffix suffix strings = [string ++ suffix | string <- strings]

selectgt5 :: [Int] -> [Int]
selectgt5 xs = [x | x <- xs, x > 5]

sumOdds :: [Int] -> Int
sumOdds xs = sum [x | x <- xs, odd x]

sumOdds' :: [Int] -> Int
sumOdds' = sum . filter odd

selectExpr :: [Int] -> [Int]
selectExpr xs = [x | x <- xs, x >= 20, x <= 50, even x]

countShorts :: [String] -> Int
countShorts palavras = sum [1 | palavra <- palavras, length palavra < 5]

calcExpr :: [Float] -> [Float]
calcExpr xs = [x' | x' <- [x^2/2 | x <- xs], x' > 10]

trSpaces :: String -> String
trSpaces string = [if c == ' ' then '-' else c | c <- string]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd tuplas = [x | (_,x) <- tuplas]

dotProd :: [Int] -> [Int] -> Int
dotProd xs ys = sum [x * y | (x,y) <- zip xs ys]
