module Step_5_6_9 where

import Control.Monad.Reader

-- Вспомним пример с базой пользователей и паролей:

type User = String
type Password = String
type UsersTable = [(User, Password)]

users = [("user", "123456"), ("x", "hi"), ("root", "123456")]

-- Реализуйте функцию, принимающую в качестве окружения UsersTable и возвращающую список пользователей, использующих пароль "123456" (в том же порядке, в котором они перечислены в базе).

-- GHCi> runReader usersWithBadPasswords [("user", "123456"), ("x", "hi"), ("root", "123456")]
-- ["user","root"]

usersWithBadPasswords   :: Reader UsersTable [User]
usersWithBadPasswords = asks $ map fst . filter ((=="123456") . snd)

tests = [
    runReader usersWithBadPasswords users  == ["user", "root"] ]