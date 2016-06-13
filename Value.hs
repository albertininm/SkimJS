module Value (Value (..)) where

import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Nil
    | GlobalVar
    | Double Double
    | Break (Maybe Id)
    | Function Id [Id] [Statement]
    | Return Value
    | Array [Value]
    | Empty
    deriving (Eq)

--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show Nil = "undefined"
  show Empty = ""
  show (Double double) = show double
  show (Function (Id name) args stmts) = "func: " ++ name ++ " args:" ++ showArgs args
  show (Return v) = show v
  show (Array b) = "[ " ++ (showArray (Array b)) ++ " ]"
  show (GlobalVar) = "variavel nao definida"

showArray :: Value -> String
showArray (Array []) = ""
showArray (Array [b]) = show b
showArray (Array (b:bs)) = (show b) ++ ", " ++ showArray (Array bs)

showArgs  ::  [Id]-> String
showArgs [] = ""
showArgs ((Id arg):xs) = show arg ++ " ; " ++showArgs xs
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
