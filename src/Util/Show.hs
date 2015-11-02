module Util.Show(
                mkString,
                mkString'
                ) where


mkString' :: String -> (a -> String) -> [a] -> String
mkString' sep f []       = ""
mkString' sep f [x]      = f x
mkString' sep f (x : xs) = f x ++ sep ++ mkString' sep f xs


mkString :: String -> String -> String -> (a -> String) -> [a] -> String
mkString open sep end f xs = open ++ mkString' sep f xs ++ end
