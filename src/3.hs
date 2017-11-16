import Data.Char
import Data.List
import Data.Maybe

special = ['*', '`', '~', '!', '@', '#', '$', '%', '^', '&']
isSpecial = (`elem` special)

encryptNumber :: Char -> Char
encryptNumber x = let y = read [x] :: Int in special !! y
encryptLower :: Char -> Char -> Char
encryptLower x y = let index = (ord x + ord y - ord 'a'); arr = cycle ['a'..'z'] in arr !! index
encrypt x y = encryptString x (cycle y)
encryptString [] _ = "" 
encryptString string@(s:xs) key@(k:xk)
        | isUpper s = [s] ++ encryptString xs key
        | isNumber s = [encryptNumber s] ++ encryptString xs key
        | otherwise = [encryptLower s k] ++ encryptString xs xk

decryptSpecial x = show (fromJust $ (elemIndex x special))
decryptLower x y = let index = (ord x - ord y + 130 - ord 'a'); arr = cycle ['a'..'z'] in arr !! index
decrypt x y = decryptString x (cycle y)
decryptString [] _ = "" 
decryptString string@(s:xs) key@(k:xk)
        | isUpper s = [s] ++ decryptString xs key
        | isSpecial s = decryptSpecial s ++ decryptString xs key
        | isLower s = [decryptLower s k] ++ decryptString xs xk
        | otherwise = ""