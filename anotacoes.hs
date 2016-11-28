--      param  param  retorno
d_AB_basic :: Int -> Int -> Int

d_AB_basic x1 x2 = abs (x2 - x1)

d_AB :: Float -> Float -> Float -> Float -> Float
d_AB x1 y1 x2 y2 | x1 == x2 = abs (x2 - x1)
                 | y1 == y2 = abs (y2 - y1)
                 | otherwise = sqrt ((x2 - x1)^2 + (y2 - y1)^2)


-- Soma recursiva
soma :: Int -> Int
soma 1 = 1
soma x = x + soma (x-1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

tipoTriangulo :: Int -> Int -> Int -> Int
tipoTriangulo a b c | (a /= b) && (a /= c) && (b /= c) = 3
                    | (a == b && a /= c) || (a == c && a /= b) || (b == c && b /= a) = 2
                    | otherwise = 1

removeFirst :: [a] -> [a]
removeFirst [] = []
removeFirst (a:[]) = []
removeFirst (a:x) = x

revertList :: [a] -> [a]
revertList [] = []
revertList (a:[]) = [a]
revertList (a:x) = revertList(x) ++ [a]

removeLast :: [a] -> [a]
removeLast [] = []
removeLast (a:[]) = []
removeLast (a:x) = a:removeLast(x)

sumEven :: [Int] -> Int
sumEven [] = 0
sumEven (a:[]) | a `mod` 2 == 0 = 1
               | otherwise = 0
sumEven (a:x) | a `mod` 2 == 0 = a + sumEven(x)
              | otherwise = sumEven(x)

sumAtEven :: [Int] -> Int
sumAtEven (a:[]) = a
sumAtEven (a:b:x) = a + b
