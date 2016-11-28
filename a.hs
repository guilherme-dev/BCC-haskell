import Data.List
import System.IO
import Data.Char

ultimo :: [Char] -> Char
ultimo [x] = x
ultimo (x:y) = ultimo y

primeiro :: [Char] -> Char
primeiro (x:y) = x

rem_ultimo :: [Char] -> [Char]
rem_ultimo [x] = []
rem_ultimo (x:y) = x:rem_ultimo y

pal :: [Char] -> Int
pal [] = 1
pal [x] = 1
pal (x:y) | x == (ultimo y) = pal (primeiro y:rem_ultimo y)
	| otherwise = 0

rem_n :: Int -> [Char] -> [Char]
rem_n 1 (a:x) = x
rem_n n [a] = []
rem_n n (a:x) = a:(rem_n (n-1) x)

to_b :: Int -> Int
to_b 0 = 0
to_b 1 = 1
to_b x | x < 4 = (div x 2 * 10) + mod x 2
	|otherwise = (to_b (div x 2))*10 + mod x 2
