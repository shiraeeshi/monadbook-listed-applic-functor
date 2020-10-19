module Lib
    (
      Listed(..)
      , unListedM
      , Swappable
      , swap
    ) where

import Control.Monad (Monad, Functor, join)
import Control.Applicative (Applicative)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Listed m a = Listed { unListed :: [m a] }

unListedM :: Monad m => Listed m a -> [m a]
unListedM (Listed l) = l

instance Functor m => Functor (Listed m) where
  fmap f (Listed x) = Listed $ fmap (fmap f) x

myjoin :: (Monad m, Swappable m) => Listed m (Listed m a) -> Listed m a
myjoin = Listed . fmap join . join . fmap swap . unListed . fmap unListed
--myjoin = fmap join . join . fmap swap . unListed . fmap unListed
--myjoin l =
--  let
--    u = fmap unListed l
--    uu = unListed u
--    j = join uu
--  in Listed j
--myjoin l =
--  let
--    -- u :: Monad m => Listed m [m a]
--    u = fmap unListedM l -- Listed m [m a]
--    -- uu :: Monad m => [m [m a]]
--    uu = unListedM u -- [m [m a]]
--    swapped = fmap swap uu -- [m (m [a])] or [[m (m a)]]
--    --fj = fmap join swapped -- [m [a]]
--    --j = join fj -- 
--    j = join swapped -- [m (m a)]
--    fj = fmap join j -- [m a]
--  in Listed fj

instance (Monad m, Swappable m) => Monad (Listed m) where
  return x = Listed $ [return x]
  l >>= f = myjoin $ fmap f l

instance Monad m => Applicative (Listed m) where
  pure x = Listed [pure x]
  lf <*> lx =
    let xs = unListedM lx
        fs = unListedM lf
        afs = [f <*> x | x <- xs, f <- fs]
    in Listed afs

--swap :: Monad m => [m a] -> m [a]
--swap [] = return []
--swap (x:xs) = (:) <$> x <*> swap xs
--          = sequence -- other possible implementation

--swap :: Monad m, Monad n => m (n a) -> n (m a)


class Monad s => Swappable s where
  swap :: Monad m => s (m a) -> m (s a)

instance Swappable [] where
  swap [] = return []
  swap (x:xs) = (:) <$> x <*> swap xs

instance Swappable Maybe where
  --swap :: Monad m => Maybe (m a) -> m (Maybe a)
  swap Nothing = return Nothing
  swap (Just x) = fmap Just x
