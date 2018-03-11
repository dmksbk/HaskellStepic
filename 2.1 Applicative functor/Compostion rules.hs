-- Just TEXT, do not try to compile it

-- Functor definition and laws
fmap  :: (a -> b) -> f a -> f b
fmap id = id
fmap (h1 . h2) = fmap h1 . fmap h2

-- Functor for getCmps
instance (Functor f, Functor g) => Functor (f |.| g) where
  -- ffmap  :: (a -> b) -> (|.| f g a) -> (|.| f g b)
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

-- Since f and g are functors:
fmap h1 (f x) = f(h1 x)
fmap

-- Functor laws for Cmps
(1) fmap id (Cmps x) == id (Cmps x)
(2) fmap h2 (fmap h1 (Cmps x)) = fmap (h2 . h1) (Cmps x)

fmap id == id
fmap id (Cmps x)              -- def fmap (Cmps)
  == Cmps $ fmap (fmap id) x  -- (1) fmap (g)
  == Cmps $ fmap id x         -- (1) fmap (f)
  == Cmps $ id x
  == Cmps x

-- Докажите выполнение второго закона функторов для композиции двух функторов:
fmap h2 (fmap h1 (Cmps x)) = fmap (h2 . h1) (Cmps x)
-- Left part:
     fmap h2 (fmap h1 (Cmps x))
  -- def fmap for (fmap h1 (Cmps x))
  == fmap h2 $ Cmps $ fmap (fmap h1) x
  -- def fmap for (fmap h2 ...)
  == Cmps $ fmap (fmap h2) (fmap (fmap h1) x)

--Right part:
     fmap (h2 . h1) (Cmps x)
  -- def fmap for Cmps
  == Cmps $ fmap (fmap (h2 . h1)) x
  -- (2) for g
  == Cmps $ fmap ((fmap h2) . (fmap h1)) x
  -- By definition of composition
  == Cmps $ fmap (fmap h2) (fmap (fmap h1) x)
-- Левая и правая части равны. Q.E.D

-- Комментарии преподавателя
-- Назовём наши функторы F и G, то есть
Cmps x :: (F |.| G) a
 
-- Для удобства перепишем левую часть закона через композицию. Теперь требуется проверить, что:
(fmap h2 . fmap h1) (Cmps x) = fmap (h2 . h1) (Cmps x)

-- для произвольных
x  :: F (G a)
h1 :: a -> b
h2 :: b -> c

-- Проверяем:
fmap h2 (fmap h1 (Cmps x)) ≡ fmap h2 (Cmps $ fmap (fmap h1) x)  -- def fmap
                           ≡ Cmps $ fmap (fmap h2) (fmap (fmap h1) x)  -- def fmap
                           ≡ Cmps $ (fmap (fmap h2) . fmap (fmap h1)) x  -- def (.)
                           = Cmps $ fmap (fmap h2 . fmap h1) x  -- Functor F
                           = Cmps $ fmap (fmap (h2 . h1)) x  -- Functor G

fmap (h2 . h1) (Cmps x) ≡ Cmps $ fmap (fmap (h2 . h1)) x  -- def fmap
