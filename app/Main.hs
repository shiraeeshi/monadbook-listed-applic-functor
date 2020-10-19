module Main where

import Lib
import Text.Read

main :: IO ()
main = do
  putStrLn "write a line and hit enter:"
  line <- getLine
  let x = listedMaybeInts line
      examined = examineListedMaybeInt x
  putStrLn $ "examined result: " ++ examined
  putStrLn $ "contents: " ++ (show (unListedM x))
  let hundr = fmap (*100) x
  putStrLn $ "hundr: " ++ (show (unListedM hundr))
  let conc = concatt <$> x <*> x <*> x
  putStrLn $ "conc: " ++ (show (unListedM conc))
  let subtr = conc >>= \i -> x >>= \j -> return $ i - j
  putStrLn $ "subtr: " ++ (show (unListedM subtr))
  print $ unListedM $ ((v <=< v) <=< v) 0
  print $ unListedM $ (v <=< (v <=< v)) 0

concatt :: Int -> Int -> Int -> Int
concatt x y z = x*100 + y*10 +z

-- use functor, applicative, monad functions

listedMaybeInts :: String -> (Listed Maybe Int)
listedMaybeInts str =
  let
    ws = words str
    mis = [readMaybe w :: Maybe Int | w <- ws]
  in Listed mis

examineListedMaybeInt :: Listed Maybe Int -> String
examineListedMaybeInt l =
  let len = length $ unListedM l
  in "length: " ++ (show len)

----------------------------

v :: Int -> Listed [] Int
v 0 = Listed [[0, 1]]
v 1 = Listed [[0], [1]]

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
f <=< g = \x -> g x >>= f
-- Alternatively
-- f <=< g = \x -> do y <- g x
--                    f y
