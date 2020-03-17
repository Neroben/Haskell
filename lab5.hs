data Product = Book String String Int|Tape String Int|Disk String String String Int deriving(Show)
--среднее значение
getAverage :: [Double] -> Double
getAverage xs = (foldr (+) 0 xs) / fromIntegral (length xs)
getAverage [] = 0
--скалярное произведение
getScalar :: [Integer] -> [Integer] -> Integer
getScalar xs ys = foldr (+) 0 (zipWith (*) xs ys)
getScalar [] [] = 0
--количество четных элементов
countEven :: [Integer] -> Int
countEven xs = length (filter even xs)
countEven [] = 0
--быстрая сортировка
quickSort :: [Integer] -> [Integer]
quickSort (x:xs) = quickSort (filter (<x) xs) ++ [x] ++ quickSort (filter (>x) xs)
quickSort [] = []
--с функцией сравнения
quickSortNew :: (a -> a -> Bool) -> [a] -> [a]
quickSortNew f (x:xs) = quickSortNew f (filter (\y -> (f y x)) xs) ++ [x] ++ quickSortNew f (filter (\y -> (not (f y x))) xs)
quickSortNew _ [] = []
-- №2
--Возвращает название товара
getTitle::Product->String
getTitle (Book a _ _) = a
getTitle (Tape a _) = a
getTitle (Disk a _ _ _) = a
--Возвращает название товаров
getTitles::[Product]->[String]
getTitles a = map (getTitle) a 
--Возвращает автора книги
bookAutor::Product->String
bookAutor (Book _ a _) = a
bookAutor (Tape _ _) = []
bookAutor (Disk _ _ _ _) = []
--Возвращает авторов книг
bookAutors::[Product]->[String]
bookAutors a = map (bookAutor) a
{-
--Возвращает товар с заданным названием---------------------------------
lookupTitle ::[Product] -> String -> [Product]
lookupTitle a s = filter function a
    where function = s == getTitle
--Возвращает список товаров с заданными названиями
lookupTitles :: [String] -> [Product] -> [Maybe Product]
lookupTitles s a = foldr (++) [] s
-}
