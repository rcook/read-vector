module Main

withFile : String -> (File -> IO (Either FileError a)) -> IO (Either FileError a)
withFile p cb = do
    Right f <- openFile p Read | Left e => pure (Left e)
    result <- cb f
    closeFile f
    pure result

parseLine : String -> List Int
parseLine = map cast . words

addLists : Num a => List a -> List a -> List a
addLists = zipWith (+)

main : IO ()
main = do
    Right (xs, ys) <- the (IO (Either FileError (List Int, List Int))) $ withFile "data.txt" $ \f => do
        Right xLine <- fGetLine f | Left e => pure (Left e)
        Right yLine <- fGetLine f | Left e => pure (Left e)
        pure (Right (parseLine xLine, parseLine yLine))
    | Left e => putStrLn ("FAILED: " ++ show e)

    putStrLn ("RESULT: xs=" ++ show xs ++ " ys=" ++ show ys)
    let result = addLists xs ys
    putStrLn ("sum=" ++ show result)