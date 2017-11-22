import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)
import qualified Text.Numeral.Language.ENG as EN
import Text.Numeral.Grammar
-- food = [(1,"Burger",5,10), (2,"Pizza",10,20)]
-- code, name, quantity, rate
-- we input code and quantity

----just return false if code with a does not exist in b

name code inventory = [y | (x,y,z,w) <- inventory, x==code]
price code inventory = [w | (x,y,z,w) <- inventory, x==code]

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
        putStrLn "NOT AVAILABLE"
        putStrLn "More items? (0/1)"
        n <- readLn :: IO Int
        checkInventory n inventory bought totalItems


queryAvailable :: Int -> Int -> [(Int,String,Int,Double)] -> [(Int,Int,String,Double, Int,Double)] -> Int -> IO()
queryAvailable code quantity inventory bought totalItems= 
    do 
        let nameItem = head (name code inventory)
        putStr nameItem 
        putStrLn " AVAILABLE"
        let newInventory = update quantity code inventory
        let priceVal = head (price code inventory)
        let newbought = bought ++ [(totalItems,code,nameItem, priceVal, quantity,  priceVal*fromIntegral(quantity) )]
        let newItems = totalItems +1
        putStrLn "More items? (0/1)"
        n <- readLn :: IO Int
        checkInventory n newInventory newbought newItems

query code quantity inventory bought totalItems
    | (notFound code inventory) = queryWrongCode code quantity inventory bought totalItems
    | (null (looku code quantity inventory)) = queryNotAvailable code quantity inventory bought totalItems
    | otherwise = queryAvailable code quantity inventory bought totalItems
 


getSum [] = 0.0
getSum ((_,_,_,_,_,value): tail) = value + getSum tail

-- printList :: [(Int,Int,String,Double,Int,Double)] -> String
printList [] = ""
printList ((x,y,z,w,a,b):tail) = (show x) ++ " | " ++ (show y) ++ " | " ++ z ++ " | " ++ (show w) ++ " | " ++ (show a) ++ " | " ++ show(b)++ "\n" ++ printList tail


checkInventory :: Int -> [(Int, String, Int, Double)] -> [(Int,Int,String,Double,Int,Double)] -> Int -> IO()
checkInventory 0 newfood bought totalItems= 
    do
      let sum = getSum bought
      putStrLn "---ALCHERINGA 2018, STALL 14: TANGO FAST FOOD CENTER---\n"
      putStrLn (printList bought)
      putStrLn (replicate 55 '-')
      putStr "Total     " 
      putStr (replicate 30 '*')
      putStr "      "
      putStrLn (show sum)
      let total = round sum :: Int
      putStr "     Rupees "
      putStrLn (read (maybe " " show (EN.us_cardinal defaultInflection total)))
      putStrLn "THANK YOU ** HAVE A NICE DAY ** PLEASE VISIT OUR STALL AGAIN"

checkInventory 1 inventory bought totalItems=
 do
  putStrLn "Enter code: "
  code <- readLn :: IO Int
  putStrLn "Enter quantity"
  quantity <- readLn :: IO Int
  query code quantity inventory bought totalItems

checkInventory _ inventory bought totalItems =
    do
        putStrLn "Please enter only (0/1)"
        putStrLn "More items? (0/1)"
        n <- readLn :: IO Int
        checkInventory n inventory bought totalItems

main = 
    do
        let totalItems = 1 :: Int
        let inventory = [(1,"Burger",5,10.0), (2,"Pizza",10,20.0)]
        checkInventory 1 inventory [] totalItems


