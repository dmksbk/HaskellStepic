module PeanoNumbers where

data Nat = Zero | Suc Nat

instance Show Nat where
	show = ('#':) . show . fromNat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat	:: Integer -> Nat
toNat x | x == 0 = Zero
		| x <  0 = error "Nat should be positive"
		| x >  0 = Suc . toNat $ x - 1

n3 = toNat 3
n4 = toNat 4
n5 = toNat 5

add :: Nat -> Nat -> Nat
add Zero n2 = n2
add (Suc n1) n2 = Suc $ add n1 n2

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Suc n1) n2 = add n2 $ mul n1 n2

fac :: Nat -> Nat
fac Zero = Suc Zero
fac n@(Suc n') = mul n $ fac n'