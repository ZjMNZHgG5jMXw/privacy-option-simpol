module Text.Dialog ( dialog ) where
import System.Console.Readline ( readline )
import Text.Printf ( printf )
dialog :: String -> [String] -> IO String
dialog pre xs = do
  line <- readline (printf "%s [%s]? " pre (comma xs))
  maybe (dialog pre xs) (guard pre xs) line
guard :: String -> [String] -> String -> IO String
guard pre xs inp
  | inp `elem` xs = return inp
  | otherwise     = dialog pre xs
comma :: [String] -> String
comma [] = ""
comma (x:xs) = foldl (\s t -> s ++ "," ++ t) x xs
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
