module Main

import Data.Vect

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

toVect : List a -> (n ** Vect n a)
toVect [] = (_ ** [])
toVect (x :: xs) =
    let (_ ** xsVect) = toVect xs
    in (_ ** x :: xsVect)

addVects : Num a => Vect n a -> Vect n a -> Vect n a
addVects = zipWith (+)

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

    let xsVect = toVect xs
    printLn xsVect

    let ysVect = toVect ys
    printLn ysVect

    -- TBD: Verify that length(xsVect) == length(ysVect)
    -- Convert to fixed-length vectors
    -- TBD: Compute sum using vector operations
    --let result = addVects xsVect ysVect
    --putStrLn "sum=???"
