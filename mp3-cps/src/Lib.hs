--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t

factk :: Integer -> (Integer -> t) -> t
factk 0 k= k 1 -- base case 0!=1
factk n k= factk (n-1) (\v->k (n*v))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] a b | even x = a x
                 |otherwise = b x
evenoddk (x:xs) a b | even x = evenoddk xs (\v->a (v+x)) b
                    |otherwise = evenoddk xs a (\v->b (v+x))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _)= True
isSimple (VarExp _)= True
--isSimple (LamExp _ _)= True
isSimple (AppExp _ _)= False
isSimple (OpExp _ e1 e2)= isSimple e1 && isSimple e2
isSimple (IfExp e1 e2 e3)= isSimple e1 && isSimple e2 && isSimple e3


--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)


--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k count = (AppExp k (IntExp i),count)
cpsExp (VarExp v) k count = (AppExp k (VarExp v),count)

--- #### Define `cpsExp` for Application Expressions

cpsExp (AppExp f x) k count | isSimple x =(AppExp (AppExp f x) k,count)
                            | otherwise = let (v,new_count) = gensym count
                                              new_k = LamExp v (AppExp(AppExp f (VarExp v)) k )
                                           in cpsExp x new_k new_count


--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k count |isSimple e1 && isSimple e2 = (AppExp k (OpExp op e1 e2),count)
                                |isSimple e1 && not(isSimple e2) = let (v,new_count)=gensym count
                                                                       new_k= LamExp v (AppExp k (OpExp op e1 (VarExp v)) )
                                                                   in cpsExp e2 new_k new_count
                                |not(isSimple e1) && isSimple e2 = let (v,new_count)=gensym count
                                                                       new_k= LamExp v (AppExp k (OpExp op (VarExp v) e2) )
                                                                   in cpsExp e1 new_k new_count
                                |not(isSimple e1) && not(isSimple e2) = let (v1,count1)=gensym count
                                                                            (v2,count2)=gensym count1
                                                                            k1= LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
                                                                            (new_e2,count3)=cpsExp e2 k1 count2
                                                                            k2=LamExp v1 new_e2
                                                                   in cpsExp e1 k2 count3

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k count |isSimple e1 = let (out_e2,count1)=cpsExp e2 k count
                                                   (out_e3,count2)=cpsExp e3 k count1
                                               in (IfExp e1 out_e2 out_e3,count2)
                                |otherwise = let (v,count1)=gensym count
                                                 (out_e2,count2)=cpsExp e2 k count1
                                                 (out_e3,count3)=cpsExp e3 k count2
                                                 new_k= LamExp v (IfExp (VarExp v) out_e2 out_e3)
                                               in cpsExp e1 new_k count3

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f args body) = let (new_body,_)=cpsExp body (VarExp "k") 0 --initialize counter:0 continucation:k
                            in Decl f (args++["k"]) new_body
