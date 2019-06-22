module List where

insertSort :: Ord a => [a] -> a -> [a] 
insertSort [] y = [y]
insertSort (x:xs) y
  | x < y = x : insertSort xs y
  | otherwise = y : x : xs

lazySortHelper :: Ord a => [a] -> [a] -> [a] 
lazySortHelper sortdedLst [] = sortdedLst
lazySortHelper sortdedLst (y:ys) = lazySortHelper (insertSort sortdedLst y) ys

{- example: sortList [] [5,9,7,1,10,3,6,8,4,2]
-}
myLazySort :: Ord a => [a] -> [a]
myLazySort lst = lazySortHelper [] lst

myConcat :: [a] -> [a] -> [a]
myConcat lst [] = lst
myConcat [] lst = lst
myConcat (x:xs) lst@(y:ys) = x : myConcat xs lst

mergeList :: Ord a => [a] -> [a] -> [a]
mergeList lst1 lst2 = myLazySort $ myConcat lst1 lst2

{- Note:

   Cannot perform the usual search as there is 
   a seemly small part of the search that handles
   the Empty or Nil branch case when searching for
   an element that does not exist in the list.

   Need new algebric data type to handle nil case
   for searching, didn't do it as I'm lazying right
   now... For now just enjoy a retracted verison
   of the search, where the function now simply
   outputs True if the element exists in the list
   and False otherwise.

   Here we go...
-}
myLazyExistenceSearch :: Eq a => [a] -> a -> Bool
myLazyExistenceSearch [] y = False
myLazyExistenceSearch (x:xs) y
  | x == y = True
  | otherwise = myLazyExistenceSearch xs y

delOne :: Eq a => [a] -> a -> [a]
delOne [] y = []
delOne (x:xs) y
  | x == y = xs
  | otherwise = x : delOne xs y

del :: Eq a => [a] -> a -> [a]
del [] y = []
del (x:xs) y
  | x == y = del xs y
  | otherwise = x : del xs y

{- Slightly Higher Order Function(s)

  myFoldl (Idea):
    (f (... (f (f accum_1 x_1) x_2)) x_n)
                  |-----|
            = initial value of
                 accumulator
       |---------------------------|
               = accum_n
          => accum is basically
             an accumulator,
             where accum_i is
             the i-th
             accumulated state
               => Think about
                  implications,
                  getting from
                  one step to
                  another, or
                  deductions,
                  to derive
                  some sort of
                  conclusion
                  given
                  something

     Consider the simple example below as an idea:
       f (f bag x1) x2 = f (put x1 in bag) x2
                       = (put x2 in bag)
                       = ...
                       = bag has 2 elemts
            => interpret what you
               want the 2 elemts
               in the bag to
               represent
               Ex: 1. Total sum of 2 elemts?
                   2. A list of 2 elemts?
                   3. Some operation on the 2 elemts?

  myFoldr (Idea):
    (f x_1 (f x_2 (... (f x_n fixedValue))))
                              |--------|
                           = initial value
           |------------------------------|
                = n-th valued state
          => basically the i-th ordred value
             result after the i-th operation
               => Think of the usual
                  mathematical steps from
                  eq'n A to eq'n B, where
                  B is the second last
                  step to get the final
                  answer

     Consider the simple example below as an idea:
       f x1 (f x2 bag) = f x1 (put x2 in bag)
                       = (put x1 in bag)
                       = ...
                       = bag has 2 elemts
            => similarly to the
               previous idea,
               interpret what you
               want the 2 elemts 
               in the bag to
               represent
               Ex: 1. Total sum of 2 elemts?
                   2. A list of 2 elemts?
                   3. Some operation on the 2 elemts?

  **Core Idea**
  Work from the fundation and expand outward to 
  conclude some sort of *Theorem* (<- don't take
  it literally, I'm interchanging *Theorem* and
  *Idea* or just *Natural Intuition*)
-}
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f accum [] = accum
myFoldl f accum (x:xs) = myFoldl f (f accum x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f fixedValue [] = fixedValue
myFoldr f fixedValue (x:xs) = f x (foldr f fixedValue xs)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

{- Used the unicode for lambda cause apparently
   it works when interpreted and cause I can
-}
mapFoldrStyle :: (a -> b) -> [a] -> [b]
mapFoldrStyle f = myFoldr (\λ -> ((f λ) :)) []

{- Or the more cooler way that everyone does it
-}
mapFoldrStyleTheNorm :: (a -> b) -> [a] -> [b]
mapFoldrStyleTheNorm f = myFoldr ((:) . f) []

{- Random list function
-}
crossProduct :: [a] -> [b] -> [(a, b)]
crossProduct [] _ = []
crossProduct (x:xs) lst = map (\a -> (x, a)) lst ++ crossProduct xs lst
