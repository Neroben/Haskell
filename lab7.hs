import System.Directory
import System.Environment
import System.IO
import Data.List
import Data.Function

--работа с файлом email
--проверяет строку
isEmail::String->Bool
isEmail [] = False
isEmail (x:xs) = if x == '@' then True else isEmail xs


--преобразует входной файл в список пар имен и адресов
nameList :: String -> [(String, String)]
nameList a = funcEmail (lines a)

--Строку в кортеж ФИО, email
funcEmail :: [String] -> [(String,String)]
funcEmail nameEmail = do
    let word = map words nameEmail
    tuple <- map (helpFuncEmail ([],[])) word
    return tuple

--Преобразование слов в кортеж первая ФИО второе email
helpFuncEmail::(String,String)->[String]->(String,String)
helpFuncEmail (a,b) [] = (a,b)
helpFuncEmail (a,b) (x:xs) = if (isEmail x) then (helpFuncEmail (a, b++" "++x) xs) else (helpFuncEmail (a++" "++x,b) xs)

fileEmail src = endToFirstArr (nameList src) --работа с файлом Email (без сортировки)

--работа с ICQ

icqList :: String -> [(String, String)]
icqList a = funcICQ (lines a)

funcICQ :: [String] -> [(String,String)]
funcICQ nameICQ = do
    let word = map words nameICQ
    tuple <- map (helpFuncICQ ([],[])) word
    return tuple

--Преобразование слов в кортеж первая Email второе ICQ
helpFuncICQ::(String,String)->[String]->(String,String)
helpFuncICQ (a,b) [] = (a,b)
helpFuncICQ (a,b) (x:xs) = if (isEmail x) then (helpFuncICQ (a ++ " " ++ x ,b) xs) else (helpFuncICQ (a , b++" "++x) xs)

fileICQ src = icqList src

--конец работы с ICQ

--работа со списком кортежей последнее слово на первое место
endToFirstArr::[(String, String)] -> [(String, String)]
endToFirstArr a = map (arrWork) a

arrWork::(String, String) -> (String, String)
--arrWork [] = []
arrWork (x,y) = ((endToFirst x),y)


--перенос последнего слова на первое место
endToFirst::String->String
endToFirst str = do
    let word = words str
    last <- endToFirstHelp word [] []
    return last


--помощь для переноса слова
endToFirstHelp::[String]->String->String->String
endToFirstHelp [] x str = x++str
endToFirstHelp (x:xs) last str = endToFirstHelp xs x str++" "++last

--сортировка списка кортежей




--работа с сайтом
formatNames :: [(String, String)] -> [(String, String)] -> String
formatNames email icq = ul $ formatNamesHelp email icq []

ul :: String -> String
ul str = "<ul>" ++ str ++ "</ul>"

li :: String -> String
li str = "<li>" ++ str ++ "</li>"

formatNamesHelp::[(String, String)]-> [(String, String)]-> String->String
formatNamesHelp [] _ str = []
formatNamesHelp (x:xs) email str = str ++ li ( (htmlName x) ++ (htmlICQ x email) ) ++ "\n" ++ (formatNamesHelp xs email str)

htmlICQ :: (String, String) -> [(String, String)] -> String
htmlICQ email (x:xs) = if ((snd email) == (fst x)) then ", ICQ: " ++ (snd x) else htmlICQ email xs
htmlICQ _ [] = ""

htmlName::(String, String)->String
htmlName (x,y) = x ++ ", <a href=\"" ++ y ++ "\">" ++ y ++ "</a>"


--создание сайта

main :: IO ()
main = do
    email <- readFile "email.txt"
    icq <- readFile "ICQ.txt"
    writeFile "index.html" (formatNames (sort(fileEmail email)) (fileICQ icq))