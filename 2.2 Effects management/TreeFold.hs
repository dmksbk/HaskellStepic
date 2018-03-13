import           MyTest

-- Для реализации свертки двоичных деревьев нужно выбрать алгоритм обхода узлов дерева (см., например, [http://en.wikipedia.org/wiki/Tree_traversal]).

-- Сделайте двоичное дерево
data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)
instance Foldable Tree where
  foldr _ ini Nil            = ini
  foldr f ini (Branch l c r) = foldr f (c `f` foldr f ini r) l

-- Представителем класса типов Foldable, реализовав симметричную стратегию (in-order traversal). Реализуйте также три другие стандартные стратегии (pre-order traversal, post-order traversal и level-order traversal), сделав типы-обертки представителями класса Foldable.

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
instance Foldable Preorder where
  foldr _ ini (PreO Nil)            = ini
  foldr f ini (PreO (Branch l c r)) = c `f` foldr f (foldr f ini r) l

newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
instance Foldable Postorder where
  foldr _ ini (PostO Nil)            = ini
  foldr f ini (PostO (Branch l c r)) = foldr f (foldr f (c `f` ini) r) l

newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)
instance Foldable Levelorder where
  foldr _ ini (LevelO Nil)            = ini
  foldr f ini (LevelO (Branch l c r)) = c `f` foldr f (foldr f ini l) r

triv = Branch (Branch Nil 'l' Nil) 'c' (Branch Nil 'r' Nil)
tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
tests1 =
  [ lbl "  3  "
  , lbl " / \\ "
  , lbl " 1  4"
  , lbl "  \\"
  , lbl "   2"
  , putStrLn ""
  , lbl "Foldable DFS Tree lcr"
  , (foldr (:) [] triv)          =?= "lcr"
  , (foldr (:) [] tree)           =?= [1,2,3,4]
  , lbl "Foldable Preorder DFS tree clr"
  , (foldr (:) [] $ PreO triv)    =?= "clr"
  , (foldr (:) [] $ PreO tree)    =?= [3,1,2,4]
  , lbl "Foldable Postorder DFS tree lrc"
  , (foldr (:) [] $ PostO triv)    =?= "lrc"
  , (foldr (:) [] $ PostO tree)   =?= [2,1,4,3]
  , lbl "Foldable LevelO BFS tree clr"
  , (foldr (:) [] $ LevelO triv)    =?= "clr"
  , (foldr (:) [] $ LevelO tree)  =?= [3,1,4,2]
  ]

main = sequence_ $ tests1
