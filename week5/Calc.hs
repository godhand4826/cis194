{-# LANGUAGE FlexibleInstances #-}

module Calc where
import Data.Map as M
import ExprT
import Parser
import qualified StackVM

eval :: ExprT -> Integer
eval e = case e of
    Lit a -> a
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
   lit = Lit
   add = Add
   mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit a = Mod7 (a `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr StackVM.Program where
    lit a = [StackVM.PushI a]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

class HasVars a where
    var :: String -> a

type VarMap = M.Map String Integer -> Maybe Integer
instance HasVars VarMap where
    var = M.lookup

instance Expr VarMap where
    lit = const . Just
    add a b m = case (a m, b m) of
                (Just v, Just v2) -> Just (v + v2)
                _ -> Nothing
    mul a b m = case (a m, b m) of
                (Just v, Just v2) -> Just (v * v2)
                _ -> Nothing

withVars :: [(String, Integer)] -> VarMap -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
