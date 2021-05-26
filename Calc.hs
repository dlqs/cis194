{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM
import qualified Data.Map as M

eval :: ExprT -> Integer

eval (Lit x) = x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr s = parseExp Lit Add Mul s >>= Just . eval

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr Integer where
  lit x = x
  add e1 e2 = e1 + e2
  mul e1 e2 = e1 * e2

instance Expr Bool where
  lit x = x > 0
  add e1 e2 = e1 || e2
  mul e1 e2 = e1 && e2

-- newtype is NOT same as type
newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

-- testExp :: Expr a => Maybe a
-- testExp = parseExp lit add mul "(3 * -4) + 5"
-- testInteger = testExp :: Maybe Integer

instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
  mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile s = (parseExp lit add mul s) :: Maybe StackVM.Program

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left x) = Nothing
eitherToMaybe (Right x) = Just x

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
              deriving (Show)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- instance Expr (M.Map String Integer -> Maybe Integer)
  -- lit =
