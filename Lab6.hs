import System.Environment
import System.IO

--распечатывает сумму чисел из консоли
num1 = do
    putStr "Введите первое значение: "
    a <- getLine
    putStr "Введите второе значение: "
    b <- getLine
    putStrLn (a ++ " + " ++ b ++ " = " ++ show((read a) + (read b)))

--Распечатывает переданные аргументы
num2 = do
    a <- getArgs
    print a

--Распечатывает весь файл
num3 = do
    fileName <- getLine
    file <- openFile fileName ReadMode
    x <- hGetContents file
    putStrLn x

--вывод n_ого кол-ва строк из файла
num4 = do
    putStr "Введите название файла: "
    fileName <- getLine
    putStr "Введите количество строк: "
    n <- getLine
    file <- openFile fileName ReadMode
    text <- hGetContents file
    output (read n) (lines text)
--вывод строк
output :: Int -> [String] -> IO ()
output 1 (x:xs) = putStrLn x
output n (x:xs) = do
                   putStrLn x
                   output (n - 1) xs

num4new = putStr "Введите название файла: " >>
    getLine >>= \fileName ->
    putStr "Введите количество строк: " >>
    getLine >>= \n ->
    openFile fileName ReadMode >>= \file ->
    hGetContents file >>= \x ->
    outputNew (read n) (lines x)
    --вывод строк
outputNew :: Int -> [String] -> IO ()
outputNew 1 (x:xs) = putStrLn x
outputNew n (x:xs) = putStrLn x >> outputNew (n - 1) xs

--задача по варианту                   
max3 = do
    putStr "Введите первое значение: "
    a <- getLine
    putStr "Введите второе значение: "
    b <- getLine
    putStr "Введите третье значение: "
    c <- getLine
    if max2 (read a) (read b) > (read c)
    then return (show (max2 (read a) (read b))) else return c

max2 :: Int -> Int -> Int
max2 a b = if a > b then a
        else b