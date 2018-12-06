module Main

-- TBD: How to make this polymorphic?
withFile : String -> (File -> IO (Either FileError (List Int, List Int))) -> IO (Either FileError (List Int, List Int))
withFile p cb = do
    Right f <- openFile p Read | Left e => pure (Left e)
    result <- cb f
    closeFile f
    pure result

parseLine : String -> List Int
parseLine = map cast . words

main : IO ()
main = do
    Right (xs, ys) <- withFile "data.txt" $ \f => do
        Right xLine <- fGetLine f | Left e => pure (Left e)
        Right yLine <- fGetLine f | Left e => pure (Left e)
        let xs = parseLine xLine
        let ys = parseLine yLine
        pure (Right (xs, ys))
    | Left e => putStrLn ("FAILED: " ++ show e)

    putStrLn ("RESULT: xs=" ++ show xs ++ " ys=" ++ show ys)
