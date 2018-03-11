{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

import           MyTest

-- Сделайте тип
newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) }
  deriving (Eq,Show)
-- представителем класса типов Functor при условии, что первые его три параметра являются функтороми:

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap u (Cmps3 x) = Cmps3 $ fmap (fmap (fmap u)) x

-- tests1 =
--   [ fmap (^2) $ Cmps3 [[[1],[2,3,4],[5,6]],[],[[7,8],[9,10,11]]] =?=
--       Cmps3 {getCmps3 = [[[1],[4,9,16],[25,36]],[],[[49,64],[81,100,121]]]}]

main =
  putStrLn . show $ fmap (^2) $ Cmps3 [[[1],[2,3,4],[5,6]],[],[[7,8],[9,10,11]]]
