printNumbers n current = do 
    let n1 = mod current 10
    let n2 = mod (div current 10) 10
    let n3 = mod (div current 100) 10
    if (n1 + n2 + n3) == n then do
        putStrLn (show current)
        if (current + 1) < 1000 then do
            printNumbers n (current+1)
         else do
            return ()
     else do
        if (current + 1) < 1000 then do
            printNumbers n (current+1)
         else do
            return ()

main = do
    putStrLn "Введите N:"
    n <- getLine
    putStrLn ""
    printNumbers (read n) 100