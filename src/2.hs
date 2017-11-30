-- :set -XDeriveGeneric
-- :set -XDeriveDataTypeable

import qualified GHC.Generics as G
import Data.Data
import qualified Data.Map as Map
import qualified Text.Numeral.Language.ENG as EN
import Text.Numeral.Grammar
import qualified Text.PrettyPrint.Tabulate as T
-- food = [(1,"Burger",5,10), (2,"Pizza",10,20)]
-- code, name, quantity, rate
-- we input code and quantity

----just return false if code with a does not exist in b


data Bought = Bought Int Int String Double Int Double deriving (Data, G.Generic)
instance T.Tabulate Bought

getname code inventory = [y | (x,y,z,w) <- inventory, x==code]
getprice code inventory = [w | (x,y,z,w) <- inventory, x==code]

notFound a b = null [(x,y,z,w)| (x,y,z,w) <- b, x == a]

looku code quantity tuples = [(x,y,z,w) | (x,y,z,w) <- tuples, x == code, z >= quantity]

update quantity key [] = []
update quantity key ((x,y,z,w):tail) | x == key, z >= quantity = (x,y,z-quantity,w):tail
                                        | otherwise = (x,y,z,w):update quantity key tail

queryWrongCode code quantity inventory bought totalItems= 
    do
        putStrLn "WRONG CODE, NO ITEM FOUND"
        putStrLn "More items? (0/1)"
        n <- readLn :: IO Int
        checkInventory n inventory bought totalItems

queryNotAvailable code quantity inventory bought totalItems = 
    do
        putStrLn (replicate 25 '-')
        putStrLn "NOT AVAILABLE"
        putStrLn (replicate 25 '-')
        putStrLn "More items? (0/1)"
        n <- readLn :: IO Int
        checkInventory n inventory bought totalItems


-- queryAvailable :: Int -> Int -> [(Int,String,Int,Double)] -> [Bought] -> Int -> IO()
queryAvailable code quantity inventory bought totalItems= 
    do 
        let nameItem = head (getname code inventory)
        putStrLn (replicate 25 '-')
        putStr nameItem 
        putStrLn "        AVAILABLE        "
        putStrLn (replicate 25 '-')
        let newInventory = update quantity code inventory
        let priceVal = head (getprice code inventory)
        let value = priceVal*fromIntegral(quantity)
        let newbought = bought ++ [Bought totalItems code nameItem priceVal quantity value]
        let newItems = totalItems +1
        putStrLn "More items? (0/1)"
        n <- readLn :: IO Int
        checkInventory n newInventory newbought newItems

query code quantity inventory bought totalItems
        | (notFound code inventory) = queryWrongCode code quantity inventory bought totalItems
        | (null (looku code quantity inventory)) = queryNotAvailable code quantity inventory bought totalItems
        | otherwise = queryAvailable code quantity inventory bought totalItems
 

getSum [] = 0.0
getSum (Bought _ _ _ _ _ value  : tail) = value + getSum tail

-- checkInventory :: Int -> [(Int, String, Int, Double)] -> [Bought] -> Int -> IO()
checkInventory 0 newfood bought totalItems= 
    do
        let sum = getSum bought
        putStrLn (replicate 55 '-')
        putStrLn "   ALCHERINGA 2018, STALL 14: TANGO FAST FOOD CENTER   \n"
        T.ppTable bought
        putStrLn (replicate 55 '-')
        putStr "Total     " 
        putStr (replicate 30 '*')
        putStr "      "
        putStrLn (show sum)
        putStrLn (replicate 55 '-')
        let total = round sum :: Int
        putStr "     Rupees "
        putStrLn (read (maybe " " show (EN.us_cardinal defaultInflection total)))
        putStrLn (replicate 55 '-')
        putStrLn "THANK YOU ** HAVE A NICE DAY ** PLEASE VISIT OUR STALL AGAIN"
        putStrLn (replicate 55 '-')

checkInventory 1 inventory bought totalItems=
    do
        putStrLn "Enter code: "
        code <- readLn :: IO Int
        putStrLn "Enter quantity"
        quantity <- readLn :: IO Int
        query code quantity inventory bought totalItems

checkInventory _ inventory bought totalItems =
    do
        putStrLn (replicate 55 '-')
        putStrLn "Please enter only (0/1)"
        putStrLn (replicate 55 '-')
        putStrLn "More items? (0/1)"
        n <- readLn :: IO Int
        checkInventory n inventory bought totalItems

main = 
    do
        let totalItems = 1 :: Int
        let inventory = [(23,"Burger",5,10.0), (40,"Pizza",10,20.0)]
        checkInventory 1 inventory [] totalItems


