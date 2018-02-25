module HiValera where

main' :: IO ()
main' = do
    putStrLn "What is your name?"
    putStr "Name: "
    name <- getLine
    if length name == 0 then main' 
    else putStrLn $ "Hi, " ++ name ++ "!"