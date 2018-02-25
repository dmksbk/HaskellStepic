module SeqA where

seqA :: Integer -> Integer
seqA n
    | n >= 0 =
        let
            seqA' (a, _, _) 0 = a
            seqA' (_, b, _) 1 = b
            seqA' (_, _, c) 2 = c
            seqA' (a, b, c) n = seqA' (b, c, b + c - 2 * a) (n-1)
        in seqA' (1, 2, 3) n
    | otherwise = error "n should be non negtiv"

main = print $ seqA 301 -- Should be 1276538859311178639666612897162414