import Data.List



--дефолт : [[(2,3),(3,5)], [(4,5)], [(4,1)]]   

--Элемент списка,  финальный список 
--На вход: [(2,3),(3,5)]  [[]]
-- на выход: [ [(2,3)], [(3,5)] ]
create_arr :: [(Int,Int)] -> [[(Int,Int)]] ->[[(Int,Int)]]
create_arr [] full_list = tail full_list
create_arr (x:xs) empty_list =  create_arr xs (empty_list ++ ((x:[]):[]))




--Получаем список [ [(2,3)], [(3,5)] ], пустой список для ответа [[(]],  элемент [(4,5)] и номер узла - 2
add_to_list :: [[(Int,Int)]] -> [[(Int,Int)]] -> [(Int,Int)] -> Int -> [[(Int,Int)]]
add_to_list [] arr _ _ = tail arr
add_to_list (x:xs) arr elem i = 
    if (fst (head x) == i) then 
        add_to_list xs (arr ++ ((x ++ elem):[])) elem i
    else 
        add_to_list xs (arr ++ (x : [])) elem i



--Полный список для графа [[(2,3),(3,5)], [(4,5)], [(4,1)]], пустой для ответа, номер итерации 
func_all_path :: [[(Int,Int)]] -> [[(Int,Int)]] -> Int -> [[(Int,Int)]] 
func_all_path [] ans _ = ans
func_all_path (x:xs) ans i =
    if (length x /= 1) then func_all_path xs (create_arr x ans) (i+1)
    else
        func_all_path xs (add_to_list ans [[]] x i) (i+1)


--Подготовленный список, список для минимума, минимальная длина
fst_min :: [[(Int,Int)]]->[(Int,Int)]->Int->[(Int,Int)]
fst_min [] ans _ = ans 
fst_min (x:xs) arr min = if ((snd_min x 0) < min) then fst_min xs x (snd_min x 0)
    else fst_min xs arr min

snd_min :: [(Int,Int)] -> Int -> Int
snd_min [] sum = sum
snd_min (x:xs) sum = snd_min xs (sum + (snd x)) 

write_path::[(Int,Int)]-> Int ->IO()
write_path [] _ = putStrLn ""
write_path (x:xs) i = do
    putStr "Вершина №"
    putStr (show i)
    putStr " - "
    putStr (show (fst x))
    putStr "; "
    write_path xs (i+1)

main :: IO()
main = do
    putStrLn "Введите граф:"
    str_arr <- getLine
    let arr = (read str_arr) :: [[(Int,Int)]]
    let ans = fst_min (func_all_path arr [[]] 1) [] 100
    putStrLn ""
    putStr "Вершина №1 - 1; "
    write_path ans 2
    putStr "Его длина равна: "
    putStrLn (show (snd_min ans 0))
    putStrLn ""
