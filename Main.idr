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

toVect : List a -> (n ** Vect n a)
toVect [] = (_ ** [])
toVect (x :: xs) =
    let (_ ** xsVect) = toVect xs
    in (_ ** x :: xsVect)

main : IO ()
main = do
    Right (xs, ys) <- withFile {a = (List Int, List Int)} "data.txt" $ \f => do
        Right xLine <- fGetLine f | Left e => pure (Left e)
        Right yLine <- fGetLine f | Left e => pure (Left e)
        pure (Right (parseLine xLine, parseLine yLine))
    | Left e => putStrLn ("FAILED: " ++ show e)

    let (xsLength ** xsVect) = toVect xs
    let (ysLength ** ysVect) = toVect ys

    case exactLength xsLength ysVect of
        Nothing => putStrLn "Vector lengths do not match"
        Just ysVect' => printLn (zipWith (+) xsVect ysVect')
