{-# LANGUAGE TypeOperators #-}

module Party where

import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e (GL el fun) = GL (e:el) (empFun e + fun)

instance Semigroup GuestList where
  GL el1 fun1 <> GL el2 fun2 = GL (el1 ++ el2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node r children)
 = f r (map (treeFold f) children)

-- combineGLs :: Employee -> [GuestList] -> GuestList

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls =
  (glCons e $ foldr (<>) mempty (map snd gls),
              foldr (<>) mempty (map fst gls))

maxFun :: Tree Employee -> GuestList
maxFun t =
  let (a, b) = treeFold nextLevel t in
  moreFun a b

main :: IO()
main = readFile "company.txt" >>= putStrLn . show . maxFun . read
