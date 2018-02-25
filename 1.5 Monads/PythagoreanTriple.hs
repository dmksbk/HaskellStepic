module PythagoreanTriple where

-- Используя монаду списка и do-нотацию, реализуйте функцию, которая принимает на вход некоторое число x и возвращает список троек (a,b,c), таких что a2+b2=c2,a>0,b>0,c>0,c≤x,a<b. Число x может быть ≤0 , на таком входе должен возвращаться пустой список. 

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = [ (b,a,c) | c <- [1..x], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2  ]

tests =
    [ pythagoreanTriple 5  == [(3,4,5)]
    , pythagoreanTriple 0  == []
    , pythagoreanTriple 10 == [(3,4,5),(6,8,10)] ]

testsCount = countCorrect ++ " out of " ++ countAll ++ " tests are correct" where
    countCorrect = show . length $ filter ( == True) tests
    countAll = show . length $ tests

-- | main function
main = do
    print tests
    putStrLn $ if and tests then "Good job!" else "Something goes wrong: " ++ testsCount