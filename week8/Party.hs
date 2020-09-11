module Party where
import Data.List
import Data.Tree
import Data.Foldable
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (empFun e+fun)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL e f) (GL e2 f2) = GL (e++e2) (f+f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f i (Node a as) = f a (map (treeFold f i) as)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subs = (a, b)
    where a = glCons boss (fold (map snd subs))
          b = fold (map fst subs)

maxFun :: Tree Employee -> GuestList
maxFun t = moreFun a b
    where (a,b) = treeFold nextLevel (mempty, mempty) t

main :: IO()
main = do
    (GL es fun) <- maxFun . read <$> readFile "company.txt"
    putStrLn $ "Total fun: " ++ show fun
    mapM_ putStrLn (sort $ map empName es)
