module Main where 
import Data.List.Split
import Data.Maybe
import Data.List
import System.Environment
import Tokens
import Grammar

eval :: Exp -> [[[String]]] -> [[String]] -> [[String]]
eval (Var x) files args = [[x]]
eval (Format x) files args | checkFormat x = [[x]]
                           | otherwise = error ("Illegal format of argument: " ++ x)

eval (Position (Format x)) files args | length (splitOn "," (unwrap(eval (Format x) files args))) == 1 = eval (Format x) files args
                                      | otherwise = error ("Illegal number of arguments: " ++ x)
eval (Position (Var x)) files args = error ("Illegal Position format of argument: " ++ x)
eval (Position _) files args = error "Illegal Position format!"

eval (Value (Var x)) files args = eval (Var x) files args
eval (Value (Format x)) files args = [[x]]
eval (Value _) files args = error "Illegal Value format!"

eval (Read (Var x) (Format y)) files args = readFilter (takeN (files !! 0) (findNumElements (unwrap (eval (Format y) files args)))) (args !! 0) 0
eval (Read (Var x) (Var y)) files args = error ("Illegal arguments after file: " ++ show x ++ " in argument: " ++ y)
eval (Read (Var x) _) files args = error ("Illegal arguments after file: " ++ show x)
eval (Read _ (Format y)) files args = error "Incorrect format of arguments in the first part of Read!"
eval (Read _ _) files args = error "Incorrect use of Read!"

eval (Equal (Position x) (Position y)) files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ "=" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (Equal (Position x) (Value y))    files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ "=" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (Equal (Value x)    (Position y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ "=" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (Equal (Value x) (Value y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ "=" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (Equal _ _) files args = error "Invalid arguments of =="

eval (NotEqual (Position x) (Position y)) files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ "!=" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (NotEqual (Position x) (Value y))    files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ "!=" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (NotEqual (Value x)    (Position y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ "!=" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (NotEqual (Value x) (Value y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ "!=" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (NotEqual _ _) files args = error "Invalid arguments of !="

eval (GRT (Position x) (Position y)) files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ ">" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (GRT (Position x) (Value y))    files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ ">" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (GRT (Value x)    (Position y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ ">" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (GRT (Value x) (Value y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ ">" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (GRT _ _) files args = error "Invalid arguments of >"

eval (GET (Position x) (Position y)) files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ ">=" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (GET (Position x) (Value y))    files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ ">=" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (GET (Value x)    (Position y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ ">=" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (GET (Value x) (Value y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ ">=" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (GET _ _) files args = error "Invalid arguments of >="

eval (LST (Position x) (Position y)) files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ "<" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (LST (Position x) (Value y))    files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ "<" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (LST (Value x)    (Position y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ "<" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (LST (Value x) (Value y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ "<" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (LST _ _) files args = error "Invalid arguments of <"

eval (LET (Position x) (Position y)) files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ "<=" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (LET (Position x) (Value y))    files args = [["pos" ++ (unwrap (eval (Position x) files args)) ++ "<=" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (LET (Value x)    (Position y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ "<=" ++ "pos" ++ (unwrap (eval (Position y) files args))]]
eval (LET (Value x) (Value y)) files args = [["val" ++ (unwrap (eval (Value x) files args)) ++ "<=" ++ "val" ++ (unwrap (eval (Value y) files args))]]
eval (LET _ _) files args = error "Invalid arguments of <="

eval (Conjunction (Read x1 y1) (Read x2 y2)) files args = conjunction (eval (Read x1 y1) files args) (eval (Read x2 y2) (drop 1 files) (drop 1 args)) (concat (scanArgs (Read x1 y1))) (concat (scanArgs (Read x2 y2)))
eval (Conjunction (Conjunction (Read x1 y1) (Read x2 y2)) (Read x3 y3)) files args = conjunction (eval (Conjunction (Read x1 y1) (Read x2 y2)) files args) (eval (Read x3 y3) (drop 2 files) (drop 2 args)) (concat (scanArgs (Conjunction (Read x1 y1) (Read x2 y2)))) (concat (scanArgs (Read x3 y3)))
eval (Conjunction (Read x3 y3) (Conjunction (Read x1 y1) (Read x2 y2))) files args = conjunction (eval (Read x3 y3) files args) (eval (Conjunction (Read x1 y1) (Read x2 y2)) (drop 1 files) (drop 1 args)) (concat (scanArgs (Read x3 y3))) (concat (scanArgs (Conjunction (Read x1 y1) (Read x2 y2))))
eval (Conjunction (Conjunction x1 y1) (Conjunction x2 y2)) files args = conjunction (eval (Conjunction x1 y1) (take (countFiles (Conjunction x1 y1)) files) (take (countFiles (Conjunction x1 y1)) args)) (eval (Conjunction x2 y2) (drop (countFiles (Conjunction x1 y1)) files) (drop (countFiles (Conjunction x1 y1)) args)) (concat (scanArgs (Conjunction x1 y1))) (concat (scanArgs (Conjunction x2 y2)))
eval (Conjunction (Read x1 y1) (Filter x2 y2)) files args = conjunction (eval (Read x1 y1) (take (countFiles (Read x1 y1)) files) (take (countFiles (Read x1 y1)) args)) (eval (Filter x2 y2) (drop (countFiles (Read x1 y1)) files) (drop (countFiles (Read x1 y1)) args)) (concat (scanArgs (Read x1 y1))) (concat (scanArgs (Filter x2 y2)))
eval (Conjunction (Filter x2 y2) (Read x1 y1)) files args = conjunction (eval (Filter x2 y2) (take (countFiles (Filter x2 y2)) files) (take (countFiles (Filter x2 y2)) args)) (eval (Read x1 y1) (drop (countFiles (Filter x2 y2)) files) (drop (countFiles (Filter x2 y2)) args)) (concat (scanArgs (Filter x2 y2))) (concat (scanArgs (Read x1 y1)))
eval (Conjunction (Read x1 y1) (Exist x2)) files args = conjunction (eval (Read x1 y1) (take (countFiles (Read x1 y1)) files) (take (countFiles (Read x1 y1)) args)) (eval (Exist x2) (drop (countFiles (Read x1 y1)) files) (drop (countFiles (Read x1 y1)) args)) (concat (scanArgs (Read x1 y1))) (concat (scanArgs (Exist x2)))
eval (Conjunction (Exist x2) (Read x1 y1)) files args = conjunction (eval (Exist x2) (take (countFiles (Exist x2)) files) (take (countFiles (Exist x2)) args)) (eval (Read x1 y1) (drop (countFiles (Exist x2)) files) (drop (countFiles (Exist x2)) args)) (concat (scanArgs (Exist x2))) (concat (scanArgs (Read x1 y1)))
eval (Conjunction (Conjunction x1 y1) (Filter x2 y2)) files args = conjunction (eval (Conjunction x1 y1) (take (countFiles (Conjunction x1 y1)) files) (take (countFiles (Conjunction x1 y1)) args)) (eval (Filter x2 y2) (drop (countFiles (Conjunction x1 y1)) files) (drop (countFiles (Conjunction x1 y1)) args)) (concat (scanArgs (Conjunction x1 y1))) (concat (scanArgs (Filter x2 y2)))
eval (Conjunction (Filter x2 y2) (Conjunction x1 y1)) files args = conjunction (eval (Filter x2 y2) (take (countFiles (Filter x2 y2)) files) (take (countFiles (Filter x2 y2)) args)) (eval (Conjunction x1 y1) (drop (countFiles (Filter x2 y2)) files) (drop (countFiles (Filter x2 y2)) args)) (concat (scanArgs (Filter x2 y2))) (concat (scanArgs (Conjunction x1 y1)))
eval (Conjunction (Conjunction x1 y1) (Exist x2)) files args = conjunction (eval (Conjunction x1 y1) (take (countFiles (Conjunction x1 y1)) files) (take (countFiles (Conjunction x1 y1)) args)) (eval (Exist x2) (drop (countFiles (Conjunction x1 y1)) files) (drop (countFiles (Conjunction x1 y1)) args)) (concat (scanArgs (Conjunction x1 y1))) (concat (scanArgs (Exist x2)))
eval (Conjunction (Exist x2) (Conjunction x1 y1)) files args = conjunction (eval (Exist x2) (take (countFiles (Exist x2)) files) (take (countFiles (Exist x2)) args)) (eval (Conjunction x1 y1) (drop (countFiles (Exist x2)) files) (drop (countFiles (Exist x2)) args)) (concat (scanArgs (Exist x2))) (concat (scanArgs (Conjunction x1 y1)))
eval (Conjunction (Filter x1 y1) (Filter x2 y2)) files args = conjunction (eval (Filter x1 y1) (take (countFiles (Filter x1 y1)) files) (take (countFiles (Filter x1 y1)) args)) (eval (Filter x2 y2) (drop (countFiles (Filter x1 y1)) files) (drop (countFiles (Filter x1 y1)) args)) (concat (scanArgs (Filter x1 y1))) (concat (scanArgs (Filter x2 y2)))
eval (Conjunction (Filter x1 y1) (Exist x2)) files args = conjunction (eval (Filter x1 y1) (take (countFiles (Filter x1 y1)) files) (take (countFiles (Filter x1 y1)) args)) (eval (Exist x2) (drop (countFiles (Filter x1 y1)) files) (drop (countFiles (Filter x1 y1)) args)) (concat (scanArgs (Filter x1 y1))) (concat (scanArgs (Exist x2)))
eval (Conjunction (Exist x2) (Filter x1 y1)) files args = conjunction (eval (Exist x2) (take (countFiles (Exist x2)) files) (take (countFiles (Exist x2)) args)) (eval (Filter x1 y1) (drop (countFiles (Exist x2)) files) (drop (countFiles (Exist x2)) args)) (concat (scanArgs (Exist x2))) (concat (scanArgs (Filter x1 y1)))
eval (Conjunction (Exist x1) (Exist x2)) files args = conjunction (eval (Exist x1) (take (countFiles (Exist x1)) files) (take (countFiles (Exist x1)) args)) (eval (Exist x2) (drop (countFiles (Exist x1)) files) (drop (countFiles (Exist x1)) args)) (destaring (concat (scanArgs (Exist x1)))) (destaring (concat (scanArgs (Exist x2))))
eval (Conjunction _ _) files args = error "Incorrect argument of conjunction!"

eval (Filter (Read x1 x2) (Equal y1 y2)) files args | checkScope (splitOn "=" (unwrap (eval (Equal y1 y2) files args))) (concat (scanArgs (Read x1 x2))) = equalFilter (eval (Read x1 x2) files args) (unwrap (eval (Equal y1 y2) files args)) (concat (scanArgs (Read x1 x2)))
                                                    | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Read x1 x2) (NotEqual y1 y2)) files args | checkScope (splitOn "!=" (unwrap (eval (NotEqual y1 y2) files args))) (concat (scanArgs (Read x1 x2))) = notEqualFilter (eval (Read x1 x2) files args) (unwrap (eval (NotEqual y1 y2) files args)) (concat (scanArgs (Read x1 x2)))
                                                       | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Read x1 x2) (GRT y1 y2)) files args | checkScope (splitOn ">" (unwrap (eval (GRT y1 y2) files args))) (concat (scanArgs (Read x1 x2))) = greaterThanFilter (eval (Read x1 x2) files args) (unwrap (eval (GRT y1 y2) files args)) (concat (scanArgs (Read x1 x2)))
                                                  | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Read x1 x2) (GET y1 y2)) files args | checkScope (splitOn ">=" (unwrap (eval (GET y1 y2) files args))) (concat (scanArgs (Read x1 x2))) = greaterOrEqualThanFilter (eval (Read x1 x2) files args) (unwrap (eval (GET y1 y2) files args)) (concat (scanArgs (Read x1 x2)))
                                                  | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Read x1 x2) (LST y1 y2)) files args | checkScope (splitOn "<" (unwrap (eval (LST y1 y2) files args))) (concat (scanArgs (Read x1 x2))) = lesserThanFilter (eval (Read x1 x2) files args) (unwrap (eval (LST y1 y2) files args)) (concat (scanArgs (Read x1 x2)))
                                                  | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Read x1 x2) (LET y1 y2)) files args | checkScope (splitOn "<=" (unwrap (eval (LET y1 y2) files args))) (concat (scanArgs (Read x1 x2))) = lesserOrEqualThanFilter (eval (Read x1 x2) files args) (unwrap (eval (LET y1 y2) files args)) (concat (scanArgs (Read x1 x2)))
                                                  | otherwise = error "Variable not in scope of '/' or doesn't exist!"
                                                  
eval (Filter (Conjunction x1 x2) (Equal y1 y2)) files args | checkScope (splitOn "=" (unwrap (eval (Equal y1 y2) files args))) (concat (scanArgs (Conjunction x1 x2))) = equalFilter (eval (Conjunction x1 x2) files args) (unwrap (eval (Equal y1 y2) files args)) (concat (scanArgs (Conjunction x1 x2)))
                                                           | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Conjunction x1 x2) (NotEqual y1 y2)) files args | checkScope (splitOn "!=" (unwrap (eval (NotEqual y1 y2) files args))) (concat (scanArgs (Conjunction x1 x2))) = notEqualFilter (eval (Conjunction x1 x2) files args) (unwrap (eval (NotEqual y1 y2) files args)) (concat (scanArgs (Conjunction x1 x2)))
                                                              | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Conjunction x1 x2) (GRT y1 y2)) files args | checkScope (splitOn ">" (unwrap (eval (GRT y1 y2) files args))) (concat (scanArgs (Conjunction x1 x2))) = greaterThanFilter (eval (Conjunction x1 x2) files args) (unwrap (eval (GRT y1 y2) files args)) (concat (scanArgs (Conjunction x1 x2)))
                                                         | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Conjunction x1 x2) (GET y1 y2)) files args | checkScope (splitOn ">=" (unwrap (eval (GET y1 y2) files args))) (concat (scanArgs (Conjunction x1 x2))) = greaterOrEqualThanFilter (eval (Conjunction x1 x2) files args) (unwrap (eval (GET y1 y2) files args)) (concat (scanArgs (Conjunction x1 x2)))
                                                         | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Conjunction x1 x2) (LST y1 y2)) files args | checkScope (splitOn "<" (unwrap (eval (LST y1 y2) files args))) (concat (scanArgs (Conjunction x1 x2))) = lesserThanFilter (eval (Conjunction x1 x2) files args) (unwrap (eval (LST y1 y2) files args)) (concat (scanArgs (Conjunction x1 x2)))
                                                         | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Conjunction x1 x2) (LET y1 y2)) files args | checkScope (splitOn "<=" (unwrap (eval (LET y1 y2) files args))) (concat (scanArgs (Conjunction x1 x2))) = lesserOrEqualThanFilter (eval (Conjunction x1 x2) files args) (unwrap (eval (LET y1 y2) files args)) (concat (scanArgs (Conjunction x1 x2)))
                                                         | otherwise = error "Variable not in scope of '/' or doesn't exist!"
                                                        
eval (Filter (Filter x1 x2) (Equal y1 y2)) files args | checkScope (splitOn "=" (unwrap (eval (Equal y1 y2) files args))) (concat (scanArgs (Filter x1 x2))) = equalFilter (eval (Filter x1 x2) files args) (unwrap (eval (Equal y1 y2) files args)) (concat (scanArgs (Filter x1 x2)))
                                                      | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Filter x1 x2) (NotEqual y1 y2)) files args | checkScope (splitOn "!=" (unwrap (eval (NotEqual y1 y2) files args))) (concat (scanArgs (Filter x1 x2))) = notEqualFilter (eval (Filter x1 x2) files args) (unwrap (eval (NotEqual y1 y2) files args)) (concat (scanArgs (Filter x1 x2)))
                                                         | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Filter x1 x2) (GRT y1 y2)) files args | checkScope (splitOn "<" (unwrap (eval (GRT y1 y2) files args))) (concat (scanArgs (Filter x1 x2))) = greaterThanFilter (eval (Filter x1 x2) files args) (unwrap (eval (GRT y1 y2) files args)) (concat (scanArgs (Filter x1 x2)))
                                                    | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Filter x1 x2) (GET y1 y2)) files args | checkScope (splitOn "<=" (unwrap (eval (GET y1 y2) files args))) (concat (scanArgs (Filter x1 x2))) = greaterOrEqualThanFilter (eval (Filter x1 x2) files args) (unwrap (eval (GET y1 y2) files args)) (concat (scanArgs (Filter x1 x2)))
                                                    | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Filter x1 x2) (LST y1 y2)) files args | checkScope (splitOn ">" (unwrap (eval (GRT y1 y2) files args))) (concat (scanArgs (Filter x1 x2))) = lesserThanFilter (eval (Filter x1 x2) files args) (unwrap (eval (LST y1 y2) files args)) (concat (scanArgs (Filter x1 x2)))
                                                    | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Filter x1 x2) (LET y1 y2)) files args | checkScope (splitOn ">=" (unwrap (eval (GET y1 y2) files args))) (concat (scanArgs (Filter x1 x2))) = lesserOrEqualThanFilter (eval (Filter x1 x2) files args) (unwrap (eval (LET y1 y2) files args)) (concat (scanArgs (Filter x1 x2)))
                                                    | otherwise = error "Variable not in scope of '/' or doesn't exist!"
                                                         
eval (Filter (Exist x) (Equal y1 y2)) files args | checkScope (splitOn "=" (unwrap (eval (Equal y1 y2) files args))) (concat (scanArgs (Exist x))) = equalFilter (eval (Exist x) files args) (unwrap (eval (Equal y1 y2) files args)) (concat (scanArgs (Exist x)))
                                                 | otherwise = error "Variable not in scope of '/' or doesn't exist!"
eval (Filter (Exist x) (NotEqual y1 y2)) files args | checkScope (splitOn "!=" (unwrap (eval (NotEqual y1 y2) files args))) (concat (scanArgs (Exist x))) = notEqualFilter (eval (Exist x) files args) (unwrap (eval (NotEqual y1 y2) files args)) (concat (scanArgs (Exist x)))
                                                    | otherwise = error "Variable not in scope of '/' or doesn't exist!"                                                    
eval (Filter (Exist x) (GRT y1 y2)) files args | checkScope (splitOn ">" (unwrap (eval (GRT y1 y2) files args))) (concat (scanArgs (Exist x))) = greaterThanFilter (eval (Exist x) files args) (unwrap (eval (GRT y1 y2) files args)) (concat (scanArgs (Exist x)))
                                               | otherwise = error "Variable not in scope of '/' or doesn't exist!"                                                    
eval (Filter (Exist x) (GET y1 y2)) files args | checkScope (splitOn ">=" (unwrap (eval (GET y1 y2) files args))) (concat (scanArgs (Exist x))) = greaterOrEqualThanFilter (eval (Exist x) files args) (unwrap (eval (GET y1 y2) files args)) (concat (scanArgs (Exist x)))
                                               | otherwise = error "Variable not in scope of '/' or doesn't exist!"                                                    
eval (Filter (Exist x) (LST y1 y2)) files args | checkScope (splitOn "<" (unwrap (eval (LST y1 y2) files args))) (concat (scanArgs (Exist x))) = lesserThanFilter (eval (Exist x) files args) (unwrap (eval (LST y1 y2) files args)) (concat (scanArgs (Exist x)))
                                               | otherwise = error "Variable not in scope of '/' or doesn't exist!"                                                    
eval (Filter (Exist x) (LET y1 y2)) files args | checkScope (splitOn "<=" (unwrap (eval (LET y1 y2) files args))) (concat (scanArgs (Exist x))) = lesserOrEqualThanFilter (eval (Exist x) files args) (unwrap (eval (LET y1 y2) files args)) (concat (scanArgs (Exist x)))
                                               | otherwise = error "Variable not in scope of '/' or doesn't exist!"
                                                    
eval (Filter _ (Equal y1 y2)) files args = error "Incorrect format of arguments in the first part of Filter!"
eval (Filter _ (NotEqual y1 y2)) files args = error "Incorrect format of arguments in the first part of Filter!"
eval (Filter _ (GRT y1 y2)) files args = error "Incorrect format of arguments in the first part of Filter!"
eval (Filter _ (GET y1 y2)) files args = error "Incorrect format of arguments in the first part of Filter!"
eval (Filter _ (LST y1 y2)) files args = error "Incorrect format of arguments in the first part of Filter!"
eval (Filter _ (LET y1 y2)) files args = error "Incorrect format of arguments in the first part of Filter!"
eval (Filter (Conjunction x1 x2) _) files args = error "Incorrect format of arguments in the second part of Filter!"
eval (Filter (Filter x1 x2) _) files args = error "Incorrect format of arguments in the second part of Filter!"
eval (Filter (Exist x) _) files args = error "Incorrect format of arguments in the second part of Filter!"
eval (Filter _ _) files args = error "Incorrect use of Filter"

eval (Exist (Read x y)) files args = exist (eval (Read x y) (take (countFiles (Read x y)) files) (take (countFiles (Read x y)) args)) (concat (scanArgs (Read x y)))
eval (Exist (Conjunction x y)) files args = exist (eval (Conjunction x y) files args) (concat (scanArgs (Conjunction x y)))
eval (Exist (Filter x y)) files args = exist (eval (Filter x y) files args) (concat (scanArgs (Filter x y)))
eval (Exist _) files args = error "Incorrect use of Exist" 

eval (Output x (Format y)) files args = output (eval x files args) (unwrap (eval (Format y) files args)) files args
eval (Output x _) files args = error "Incorrect format of arguments in the second part of Output!"

main = do 
    arguments <- getArgs
    arg <- readFile (head arguments)
    let tokens = parseKUCI (alexScanTokens arg)
    let files = scanFiles tokens
    let args = scanArgs tokens
    lists <- mapM (reader) files
    mapM_ putStrLn (map (concat') (sort (eval tokens lists args)))

reader :: String -> IO [[String]]
reader x = do
   args <- readFile x
   let linesOfFiles = lines args
   return (splitter linesOfFiles)

--returns an element which is in double [] brackets
unwrap :: [[a]] -> a
unwrap x = head (head x)

--concatenates strings and puts , between the elements
concat' :: [String] -> String
concat' [] = []
concat' (x:[]) = x
concat' (x:xs) = x ++ "," ++ concat' xs

--checks if the format of the arguments is acceptable
checkFormat :: String -> Bool
checkFormat x | isInfixOf "x*" x && x /= "x*" = False
              | '*' `elem` x && count 'x' x == length (transform' x) &&  length (transform' x) == length (splitOn "," x) - 1 = True
              | count 'x' x == length (transform x) && length (transform x) == length (splitOn "," x) = True
              | otherwise = False

--transforms variables into indexes 
--transform "x1,x2,x3" -> [1,2,3]
transform :: String -> [Int]
transform x | not ('*' `elem` x) = map (read::String->Int) (removeItem "" (removeX (splitOn "," x))) 
            | otherwise = [-1]

--removeX ["x1","x2","x3"] -> ["1","2","3"]
removeX :: [String] -> [String]
removeX [] = []
removeX (x:xs) = [ x' | x' <- x, not (x' `elem` "x")] : removeX xs

--returns just the integers from a list of arguments             
transform' :: String -> [Int]
transform' x = map (read::String->Int) (removeItem "" (removeItem "*" (removeX (splitOn "," x))))

--removes an item from a list    
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

--returns the number of elements to be taken from a file
--if -1, take all elements
findNumElements :: String -> Int
findNumElements xs | not ('*' `elem` xs) = count 'x' xs
                   | otherwise = -1

--takes a list of strings and returns a list of lists of substrings splited on ","
splitter :: [String] -> [[String]]
splitter [] = []
splitter (x:xs) = splitOn "," x : splitter xs

--takes a list of lists and returns a list of lists with only the first n elements
--takeN [["a","b"],["c","d"]] 1 -> [["a"],["c"]]
takeN :: [[String]] -> Int -> [[String]]
takeN [] _ = []
takeN xs (-1) | checkArity xs = xs
              | otherwise = error "Unequal number of arguments in a file!"
takeN xs n | checkLength xs n = map (take n) xs
           | otherwise = error ("No element at index " ++ show n ++ "!")

--checks the length of a list to given integer
--checkLength :: Foldable t => [t a] -> Int -> Bool           
checkLength [] _ = True      
checkLength (x:xs) n | length x < n = False
                     | otherwise = checkLength xs n

--checks the arity of relation
--checkArity :: Foldable t => [t a] -> Bool                     
checkArity [] = True                   
checkArity (x:[]) = True                     
checkArity (x:y:xs) | length x == length y = checkArity (y:xs)
                    | otherwise = False

--checks if variable is in the scope
--checkScope :: Foldable t => [String] -> t String -> Bool
checkScope [] _ = True               
checkScope (x:xs) args | take 3 x == "pos" && not ((drop 3 x) `elem` args) = False
                       | otherwise = checkScope xs args 

--filters the relation variables when reading a file
readFilter :: Eq a => [[a]] -> [String] -> Int -> [[a]]
readFilter ls [] _ = ls
readFilter ls (x:xs) n | x /= "x*" && x `elem` xs = readFilter (myFilter ls n (n + 1 + (fromJust (elemIndex x xs)))) xs (n + 1)
                       | otherwise = readFilter ls xs (n + 1)

--removes * element from list of elements
destaring :: [String] -> [String]
destaring [] = []
destaring (x:xs) | x == "*" = destaring xs
                 | otherwise = x : destaring xs
                                          
--counts occurrences of a char in string
count :: Eq a => a -> [a] -> Int
count x xs = length [a | a <- xs, a == x]

--scan an AST and returns list of the names of the files that need to be read
scanFiles :: Exp -> [String]
scanFiles (Var _) = []
scanFiles (Format _) = []
scanFiles (Position _) = []
scanFiles (Value _) = []
scanFiles (Read (Var x) (Format y)) = [x]
scanFiles (Read _ _) = error "Invalid file!" 
scanFiles (Equal _ _) = []
scanFiles (Filter x _) = scanFiles x
scanFiles (Conjunction x y) = scanFiles x ++ scanFiles y
scanFiles (Exist x) = scanFiles x
scanFiles (Output x _) = scanFiles x

--scan an AST and return list of lists where each list contains the arguments of a file
scanArgs :: Exp -> [[String]]
scanArgs (Var _) = []
scanArgs (Format _) = []
scanArgs (Position _) = []
scanArgs (Value _) = []
scanArgs (Read (Var x) (Format y)) = [splitOn "," (unwrap (eval (Format y) [[[]]] [[]]))]
scanArgs (Read _ y) = error "Invalid file!" 
scanArgs (Equal _ _) = []
scanArgs (Filter x _) = scanArgs x
scanArgs (Conjunction x y) = scanArgs x ++ scanArgs y
scanArgs (Exist x) = scanArgs x
scanArgs (Output x _) = scanArgs x

--scans an AST and returns the number of files in it
countFiles :: Exp -> Int
countFiles (Var _) = 0
countFiles (Format _) = 0
countFiles (Position _) = 0
countFiles (Value _) = 0
countFiles (Read (Var x) y) = 1
countFiles (Equal _ _) = 0
countFiles (Filter x y) = countFiles x + countFiles y
countFiles (Conjunction x y) = countFiles x + countFiles y
countFiles (Exist x) = countFiles x
countFiles (Output x y) = countFiles x

----------------------Conjunction----------------------

--conjunction [["a","b","c"],["a","b","s"]] [["a","c","d"],["b","s","d"]] ["x1","x2","x3"] ["x1","x4","x5"] -> [["a","b","c","a","c","d"],["a","b","s","a","c","d"]]
conjunction :: [[String]] -> [[String]] -> [String] -> [String] -> [[String]]
conjunction xs ys x y | not ("*" `elem` x) && not ("*" `elem` y) = conjunctionFilter (conjunction' xs ys) (x) (y) 0
                      | otherwise = merge (conjunctionFilter (conjunction' listX listY) (x) (y) 0)
                      where listX = map (mapZ (lengthOfZ (concat x) xs) x) xs
                            listY = map (mapZ (lengthOfZ (concat y) ys) y) ys

--filters the result of conjunction by a constraint
--conjunctionFilter [["a","b","c","b","c","d"],["a","b","s","b","c","d"],["a","s","c","b","c","d"]] ["x1","x2","x3"] ["x2","x3","x4"] 0 -> [["a","b","c","b","c","d"]]
conjunctionFilter :: Eq a => [[a]] -> [[Char]] -> [[Char]] -> Int -> [[a]]
conjunctionFilter ls [] _ _ = ls
conjunctionFilter ls (x:xs) ys n | x /= "x*" && x `elem` ys = conjunctionFilter (myFilter ls n (length (x:xs) + n + (fromJust (elemIndex x ys)))) xs ys (n + 1)
                                 | otherwise = conjunctionFilter ls xs ys (n + 1)

--returns only lits where elements on positions n and m are equal
myFilter :: Eq a => [[a]] -> Int -> Int -> [[a]]
myFilter xs n m = [x | x <- xs, (x !! n) == (x !! m) ]

--takes two lists of lists and returns a list of every list of the first list concatenated with every list of the second list
conjunction' :: [[a]] -> [[a]] -> [[a]]
conjunction' [] [] = []
conjunction' xs ys =  merge [ [a,b] | a <- xs, b <- ys ]

--concatenates list of lists into a single list
--merge [["a"],["b"],["c"]] -> ["a","b","c"]
--merge :: Foldable t => [t [a]] -> [[a]]
merge [] = []
merge (x:xs) = concat x : merge xs

----------------------Filter----------------------

--Filter list of lists by given condition 
--takes a list of lists with values to be filtered, condition and list of lists of arguments of files

equalFilter :: [[String]] -> String -> [String] -> [[String]]
equalFilter xs y args | (take 3 exp1) == "pos" && (take 3 exp2) == "pos" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) == (x !! (fromJust(elemIndex (check exp2) args))))) xs
                      | (take 3 exp1) == "pos" && (take 3 exp2) == "val" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) == (check exp2))) xs
                      | (take 3 exp1) == "val" && (take 3 exp2) == "pos" = filter(\x -> (check exp1) == (x !! (fromJust(elemIndex (check exp2) args)))) xs
                      | (take 3 exp1) == "val" && (take 3 exp2) == "val" = filter(\x -> (check exp1) == (check exp2)) xs
                      where exp1 = head (splitOn "=" y)
                            exp2 = toes (splitOn "=" y)

notEqualFilter :: [[String]] -> String -> [String] -> [[String]]
notEqualFilter xs y args | (take 3 exp1) == "pos" && (take 3 exp2) == "pos" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) /= (x !! (fromJust(elemIndex (check exp2) args))))) xs
                         | (take 3 exp1) == "pos" && (take 3 exp2) == "val" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) /= (check exp2))) xs
                         | (take 3 exp1) == "val" && (take 3 exp2) == "pos" = filter(\x -> (check exp1) /= (x !! (fromJust(elemIndex (check exp2) args)))) xs
                         | (take 3 exp1) == "val" && (take 3 exp2) == "val" = filter(\x -> (check exp1) /= (check exp2)) xs
                         where exp1 = head (splitOn "!=" y)
                               exp2 = toes (splitOn "!=" y)

greaterThanFilter :: [[String]] -> String -> [String] -> [[String]]
greaterThanFilter xs y args | (take 3 exp1) == "pos" && (take 3 exp2) == "pos" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) > (x !! (fromJust(elemIndex (check exp2) args))))) xs
                            | (take 3 exp1) == "pos" && (take 3 exp2) == "val" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) > (check exp2))) xs
                            | (take 3 exp1) == "val" && (take 3 exp2) == "pos" = filter(\x -> (check exp1) > (x !! (fromJust(elemIndex (check exp2) args)))) xs
                            | (take 3 exp1) == "val" && (take 3 exp2) == "val" = filter(\x -> (check exp1) > (check exp2)) xs
                            where exp1 = head (splitOn ">" y)
                                  exp2 = toes (splitOn ">" y)

greaterOrEqualThanFilter :: [[String]] -> String -> [String] -> [[String]]
greaterOrEqualThanFilter xs y args | (take 3 exp1) == "pos" && (take 3 exp2) == "pos" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) >= (x !! (fromJust(elemIndex (check exp2) args))))) xs
                            | (take 3 exp1) == "pos" && (take 3 exp2) == "val" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) >= (check exp2))) xs
                            | (take 3 exp1) == "val" && (take 3 exp2) == "pos" = filter(\x -> (check exp1) >= (x !! (fromJust(elemIndex (check exp2) args)))) xs
                            | (take 3 exp1) == "val" && (take 3 exp2) == "val" = filter(\x -> (check exp1) >= (check exp2)) xs
                            where exp1 = head (splitOn ">=" y)
                                  exp2 = toes (splitOn ">=" y)

lesserThanFilter :: [[String]] -> String -> [String] -> [[String]]
lesserThanFilter xs y args | (take 3 exp1) == "pos" && (take 3 exp2) == "pos" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) < (x !! (fromJust(elemIndex (check exp2) args))))) xs
                           | (take 3 exp1) == "pos" && (take 3 exp2) == "val" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) < (check exp2))) xs
                           | (take 3 exp1) == "val" && (take 3 exp2) == "pos" = filter(\x -> (check exp1) < (x !! (fromJust(elemIndex (check exp2) args)))) xs
                           | (take 3 exp1) == "val" && (take 3 exp2) == "val" = filter(\x -> (check exp1) < (check exp2)) xs
                           where exp1 = head (splitOn "<" y)
                                 exp2 = toes (splitOn "<" y)

lesserOrEqualThanFilter :: [[String]] -> String -> [String] -> [[String]]
lesserOrEqualThanFilter xs y args | (take 3 exp1) == "pos" && (take 3 exp2) == "pos" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) <= (x !! (fromJust(elemIndex (check exp2) args))))) xs
                           | (take 3 exp1) == "pos" && (take 3 exp2) == "val" = filter(\x -> (x !! (fromJust(elemIndex (check exp1) args)) <= (check exp2))) xs
                           | (take 3 exp1) == "val" && (take 3 exp2) == "pos" = filter(\x -> (check exp1) <= (x !! (fromJust(elemIndex (check exp2) args)))) xs
                           | (take 3 exp1) == "val" && (take 3 exp2) == "val" = filter(\x -> (check exp1) <= (check exp2)) xs
                           where exp1 = head (splitOn "<=" y)
                                 exp2 = toes (splitOn "<=" y)                                      
                         
--checks the type of argument
--check "posx3" -> "x3"
--check "valGosho" -> "Gosho"
check :: String -> String
check x | (take 3 x) == "pos" = drop 3 x
        | (take 3 x) == "val" = drop 3 x

--like head but the opposite 
--toes ["1","3","a"] -> "a"
toes :: [a] -> a
toes xs = xs !! (length xs -1)

----------------------Output----------------------

--output [["1","2","3","4","5","6","7","8"],["8","7","6","5","4","3","2","1"]] "x1,x3,x7" [[["1","2"],["8","7"]],[["3","4"],["6","5"]],[["5","6"],["4","3"]],[["7","8"],["2","1"]]] [["x1","x2"],["x3","x4"],["x5","x6"],["x7","x8"]]
output [] _ _ _ = []
output (x:xs) y files args | y == "*" = error "Incorrect output argument '*'! Try x*"
                           | not ("x*" `elem` ys) = map (take' x files args) (map (search args 0) ys) : output xs y files args
                           | otherwise = x : output xs y files args
                           where ys = (splitOn "," y)

--takes an element given by tuple value (Int,Int) where the first Int is number of file and the second Int is the position of element in the file
take' :: [String] -> [[[String]]] -> [[String]] -> (Int,Int) -> String
take' ys xs args (x1,x2) | x1 /= -1 && x2 /= -1 = ys !! (calcPosition x1 args + x2 - (countNumStars args (x1,x2)))
                         | otherwise = ""

--calculates the position of the element 
calcPosition n (x:xs) | n /= 0 = length x + calcPosition (n - 1) xs
                | otherwise = 0

--finds the element and returns its position in tuple (Int,Int) where the first Int is number of file and the second Int is the position of element in the file 
search [] _ x | x == "*" = (-1,-1)
              | otherwise = error ("No such element as " ++ x)
search (y:ys) n x |  x /= "*" && x `elem` y = (n,(fromJust (elemIndex x y)))
                  | otherwise = search ys (n + 1) x

--counts the number of * before an element
countNumStars :: [[String]] -> (Int,Int) -> Int
countNumStars args (x1,x2) = count "*" (take (fromJust (elemIndex ((args !! x1) !! x2) (concat args))) (concat args))

----------------------Exist----------------------
   
--exist [["a","b","b","c"],["a","b","d","e"]] ["x1","*","x2"] -> [["a","c"],["a","e"]]
exist :: [[String]] -> [String] -> [[String]]
exist ys x | (lengthOfZ (concat x) ys) /= 0 = map (merge) (map (takedropZ (indexOfZ (x) 0) 0) (map (mapZ (lengthOfZ (concat x) ys) (x)) ys))
           | otherwise = [[]]

--remove element at given index
--takedropZ [2] 0 ["a","b","b","c"] -> ["a","b","c"]
takedropZ :: [Int] -> Int -> [a] -> [a]
takedropZ [] m xs = xs
takedropZ (n:ns) m xs = takedropZ ns (m + 1) ((take (n - m) xs) ++ (drop (n - m + 1) xs))

--maps z to list of values
--mapZ 2 ["x1","*","x2"] ["a","b","b","c"] -> [["a"],["b","b"],["c"]]
mapZ :: Int -> [String] -> [String] -> [[String]]
mapZ _ [] [] = []
mapZ _ _ [] = []
mapZ _ [] _ = []
mapZ z (x:xs) (y:ys) | x /= "*" = [y] : mapZ z xs ys 
                     | x == "*" = take z (y:ys) : mapZ z xs (drop z (y:ys))

--returns what the length of * should be
--lengthOfZ "x1,*,x3" [["a","b","b","c"],["a","b","d","e"]] -> 2
lengthOfZ :: String -> [[String]] -> Int
lengthOfZ _ [] = 0
lengthOfZ x (y:ys) | count '*' x > 1 = ((length y) - (count 'x' x)) `div` (count '*' x)
                   | otherwise = ((length y) - (count 'x' x))

--returns the index of z
--indexOfZ ["x1","*","x2"] 0 -> 1
indexOfZ :: [String] -> Int -> [Int]
indexOfZ [] _ = []
indexOfZ (x:xs) n | x == "*" = n : indexOfZ xs (n + 1) 
                  | otherwise = indexOfZ xs (n + 1) 
