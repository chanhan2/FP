module Prime where

isPrimeHelper :: Int -> Int -> Bool
isPrimeHelper n x 
  | x == 1 = True
  | otherwise = ((mod n x) /= 0) && (isPrimeHelper n (x - 1))

isPrime :: Int -> Bool
isPrime n
  | n < 1 = False   -- unsupported branch
  | n == 1 = True
  | otherwise = isPrimeHelper n (n - 1)

getListOfFirstNPrimes :: Int -> [Int]
getListOfFirstNPrimes n
  | n < 1 = []      -- unsupported branch
  | otherwise = filter (\p -> isPrime p) [1..n]

listOfPrimeFractors :: Int -> [Int]
listOfPrimeFractors n
  | n < 1 = [-1]    -- unsupported branch
  | otherwise = filter (\p -> mod n p == 0) $ reverse (getListOfFirstNPrimes n)

expont :: Int -> Int -> Int
expont b n
  | n <= 0 = 1
  | otherwise = b * expont b (n-1)

{- *******************************************************
   *                The weird part now....               *
   *******************************************************
   Note:

   The following applies to the functions below.

   There is no use of any new algebric data type as there is
   no error checking nor error handling. There exists major
   issues when dealing with -inf < n,p < 1, where n,p are
   Integers.

   Right now I'm lazy and I don't wanna work on this....

   *Reminder: p is interpretted as an Integer prime as usual
-}
numOfPrimeDivisions :: Int -> Int -> Int -> (Int, (Int, Int))
numOfPrimeDivisions accum n p     {-   <---- very questionable and poorly thoughtout choice   -}
  | n == 1 = (1,(0,1))     {-   <----   clear indication something is wrong with this function already in this section...  -}
  | m /= 0 = (accum,(round ((fromIntegral n) / (fromIntegral exp1)),p))
  | otherwise = numOfPrimeDivisions (accum + 1) n p
 where
   exp1 = expont p accum
   exp2 = expont p (accum + 1)
   m = mod n exp2

fractorHelper :: [Int] -> Int -> [(Int, Int)]
fractorHelper [] _ = []
fractorHelper (p:ps) n = (base, exp) : fractorHelper ps val
 where
   elemt = numOfPrimeDivisions 1 n p
   exp = (\(e,_) -> e) elemt
   val = (\(_,(v,_)) -> v) elemt
   base = (\(_,(_,b)) -> b) elemt

fractor :: Int -> [(Int, Int)] 
fractor n
  | n == 0 = []
  | otherwise = fractorHelper (listOfPrimeFractors n) n

fractorToListHelper :: (Int, Int) -> [Int]
fractorToListHelper (base,exp)
  | exp <= 0 = []
  | otherwise = base : fractorToListHelper (base,(exp-1))

fractorToList :: [(Int, Int)] -> [Int]
fractorToList [] = []
fractorToList (p:ps) = (fractorToListHelper p) ++ fractorToList ps

getListOfPrimeFractors :: Int -> [Int]
getListOfPrimeFractors = fractorToList . fractor

{- example:

  *Prime> let nums = [1,2,5,17,19,25,50,70,75,90,99,100]   -- consider any list of numbers like shown here
  *Prime> let lst = map getListOfPrimeFractors nums
  *Prime> map primeFractorMultiplication lst
-}
primeFractorMultiplication :: [Int] -> Int
primeFractorMultiplication [] = 1
primeFractorMultiplication (p:ps) = (*) p $ primeFractorMultiplication ps

{- The following two functions are more or less the same...

   *** |< BUT! >| ****

   1. DIFFERENT FUNCTION SIGNATURE AND IS APPLIED VERY IN A 
      VERY DIFFERENT MANNER!!!

   2. ONE DOES MORE WORK, WHILE THE OTHER ONLY CONSIDERS
      THE MAIN IDEA!!!
      (CONSIDER THE FOLLOWING QUESTIONS!!:
       1. (ONE IS OR BOTH ARE) MORE OR LESS MODULAR???
       2. ONE IS MORE FUNCTIONAL???
       3. ONE IS READABLE???
       4. ONE IS MORE EASILY INTERPRETED???
       5. (ONE IS OR BOTH ARE) [EASILY?] MAINTAINABLE???
       6. (ONE IS OR BOTH ARE) ABLE TO DRIVE THE IDEA
          HOME???
            -> DEFINES THE FUNCTION AS THROUGH
               BEHAVIOUR AND NOT THROUGH GENERALIZED
               SPECIFICS?????
       7. WHICH ONE IS MORE CLEARER???
          (NOT TALKING ABOUT THE LOGICAL IMPLICATIONS THAT
           FOLLOW FROM THE SEQUENTIAL STATES OF APPLYING
           THE N NUMBBERS OF OPERATIONS THROUGH FUNCTION
           CALLS!!!

           BUT ONLY THE IDEA OF WHAT IS **EXPECTED!!!**

           FOR EXAMPLE:
           f_1 <inpt> -> f_2 <inpt> -> ... -> f_N <inpt> -> outpt

             *NOT TALKING ABOUT THE SIMPLICITY NOR THE CHAIN
              OF OPERATIONS LIKE THE DIAGRAM TO ILLUSTRATED
              ABOVE*)
       8. (ONE IS OR BOTH ARE) CAN BE EASILY EXPLAINED WHAT
          THE FUNCTION DOES WITHOUT USING ANY SPECIFICS WHEN
          AND JUST THE IDEA????
          (HINT: TRY EXPLAINING WHAT THE FUNCTION DOES IN FULL
                 DETAIL WITHOUT USING ANY SPECIFICS, NO COMPLEX
                 CONCEPTS AND USING EVERY DAY NORMAL WORDS!!!
                 AND USE THAT TO HAVE SOMEONE WITH NO BACKGROUND
                 KNOWLEDGE TO UNDERSTAND EXACTLY WHAT THE
                 FUNCTION DOES BUT DOES NOT NEED TO KNOW HOW IT 
                 DOES IT!!!))
-}
getFirstPrimeMatch :: Int -> [Int] -> Int
getFirstPrimeMatch _ [] = 1
getFirstPrimeMatch n (p:ps)
  | n == p = p
  | otherwise = getFirstPrimeMatch n ps

getFirstPrimeFactorMatch :: [Int] -> [Int] -> Int
getFirstPrimeFactorMatch [] _ = -1    -- unsupported branch
getFirstPrimeFactorMatch _ [] = -1    -- unsupported branch
getFirstPrimeFactorMatch (p1:p1s) (p2:p2s)
  | p1 == 1 || p2 == 1 = 1
  | p1 /= p2 && p1 > p2 = getFirstPrimeFactorMatch p1s (p2:p2s)
  | p1 /= p2 && p1 < p2 = getFirstPrimeFactorMatch (p1:p1s) p2s
  | otherwise = p1

commonFactor :: (Int -> Bool) -> [Int] -> [Int] -> [Int]
commonFactor cond (p1:p1s) (p2:p2s) = filter cond $ map getCommonPrimes (p1:p1s)
 where
   getCommonPrimes = (\p -> getFirstPrimeMatch p (p2:p2s))

isRelativePrime :: Int -> Int -> Bool
isRelativePrime n1 n2 = null fcn
 where
   p1 = getListOfPrimeFractors n1
   p2 = getListOfPrimeFractors n2
   fcn = commonFactor (\p -> p /= 1) p1 p2

intersect :: [Int] -> [Int] -> [Int]
intersect _ [] = []    -- unsupported branch
intersect [] _ = []    -- unsupported branch
intersect (p1:p1s) (p2:p2s)
  | p1 == 1 || p2 == 1 = [1]
  | p1 /= p2 && p1 > p2 = intersect p1s (p2:p2s)
  | p1 /= p2 && p1 < p2 = intersect (p1:p1s) p2s
  | otherwise = p1 : intersect p1s p2s

commonDivisor :: ([Int] -> [Int]) -> ([Int] -> [Int] -> Int) -> Int -> Int -> Int
commonDivisor f1 f2 n1 n2 = f2 p1 p2
 where
   p1 = f1 $ getListOfPrimeFractors n1
   p2 = f1 $ getListOfPrimeFractors n2

fstElmt :: [Int] -> Int
fstElmt [] = 1
fstElmt (p:ps) = p

gcfPrime :: Int -> Int -> Int
{- The following is equivalent with the one already provided:
   gcfPrime = commonDivisor (\a -> a) getFirstPrimeFactorMatch
-}
gcfPrime = commonDivisor (\a -> a) fcn
 where
   fcn = (\p1 p2 -> fstElmt $ commonFactor (\p -> p /= 1) p1 p2)

lcfPrime :: Int -> Int -> Int
{- The following is equivalent with the one already provided:
   lcfPrime = commonDivisor ((drop 1) . reverse) getFirstPrimeFactorMatch 
-}
lcfPrime = commonDivisor ((drop 1) . reverse) fcn
 where
   fcn = (\p1 p2 -> fstElmt $ commonFactor (\p -> p /= 1) p1 p2)

myGcd :: Int -> Int -> Int
myGcd = commonDivisor (\a -> a) fcn
 where
   fcn = (\p1 p2 -> primeFractorMultiplication $ intersect p1 p2)

myLcd :: Int -> Int -> Int
myLcd n1 n2 = round calculatedLcd
 where
   calculatedLcd = (fromIntegral (n1 * n2)) / (fromIntegral (myGcd n1 n2))
{- *******************************************************
   *                      The End...                     *
   *******************************************************
-}

{- Euclidean Algorithm for gcd
-}
gcdEucliean :: Int -> Int -> Int
gcdEucliean n1 n2
  | n2 == 0 = n1
  | otherwise = gcdEucliean n2 (n1 `mod` n2)

{- The normal way people always calculate the lcd
-}
lcdStandard :: Int -> Int -> Int
lcdStandard n1 n2 = round calculatedLcd
 where
   calculatedLcd = (fromIntegral (n1 * n2)) / (fromIntegral (gcdEucliean n1 n2))
