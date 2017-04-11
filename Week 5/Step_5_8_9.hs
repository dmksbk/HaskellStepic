module Step_5_8_9 where

import Control.Monad.Writer
import Control.Monad.State

-- Теперь убедимся, что с помощью монады State можно эмулировать монаду Writer.

-- Напишите функцию writerToState, «поднимающую» вычисление из монады Writer в монаду State:

--GHCi> runState (writerToState $ tell "world") "hello,"
--((),"hello,world")
--GHCi> runState (writerToState $ tell "world") mempty
--((),"world")

--Обратите внимание на то, что при работе с монадой Writer предполагается, что изначально лог пуст (точнее, что в нём лежит нейтральный элемент моноида), поскольку интерфейс монады просто не позволяет задать стартовое значение. Монада State же начальное состояние (оно же стартовое значение в логе) задать позволяет.

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = state $ \w -> let (v, w') = runWriter m in (v, w `mappend` w')

-- writerToState m = state . runWriter m

tests = 
    [ runState (writerToState $ tell "world") "hello," == ((),"hello,world")
    , runState (writerToState $ tell "world") mempty   == ((),"world")
    ]