import Test.QuickCheck
-----------------------------------------------------------------------------------------  2  -------------------------------------------------------------------------------

----------------------------------------------------------------------------------------  2.1  ------------------------------------------------------------------------------
-- Recursive function f using memoization
f :: Integer -> Integer
f n = old_values !! fromIntegral n  -- Get the nth value from the memoized list
  where
    old_values = map f'' [0..]       -- Infinite list storing computed values of f
    f'' 0 = 0                        -- Base case: f(0) = 0
    f'' 1 = 1                        -- Base case: f(1) = 1
    f'' n = sum [old_values !! k | k <- [0..fromIntegral n - 1]] 
    -- Recursive case: f(n) = sum of all previous f(k) for k = 0 to n-1

-- Closed-form function f' that computes the same result more efficiently
f' :: Integer -> Integer
f' n = 2 ^ (n - 2)  -- f'(n) = 2^(n-2), valid for n >= 2
----------------------------------------------------------------------------------------  2.2  ------------------------------------------------------------------------------

prop_Equalff' :: Integer -> Property
prop_Equalff' n = n < 2 || f n == f' n ==> 
  classify (n < 0) "n < 0" $ 
  classify (n == 0 && n == 1) "n < 2" $ 
  n < 2 || f n == f' n

----------------------------------------------------------------------------------------  2.3  ------------------------------------------------------------------------------
runTests2 :: IO ()
runTests2 = do
  putStrLn "f(n) == f'(n) :"
  quickCheck prop_Equalff'
  verboseCheck prop_Equalff'

-----------------------------------------------------------------------------------------  3  -------------------------------------------------------------------------------

----------------------------------------------------------------------------------------  3.1  ------------------------------------------------------------------------------
-- Recursive Fibonacci function using memoization
fib :: Integer -> Integer
fib n = old_values !! fromIntegral n -- Get the nth Fibonacci number from the memoized list.
  where
    old_values = map fib' [0..] -- Infinite list storing computed values of Fibonacci numbers.
    fib' 0 = 0
    fib' 1 = 1
    fib' n = old_values !! fromIntegral (n - 1) + old_values !! fromIntegral (n - 2) -- Recursive case: fib(n) = fib(n-1) + fib(n-2)

----------------------------------------------------------------------------------------  3.2  ------------------------------------------------------------------------------
phi :: Double
phi = (1 + sqrt 5) / 2

-- Calculates the ratio between the (n+1)th and nth Fibonacci numbers.
fibRatio :: Integer -> Double
fibRatio n = fromInteger (fib (n + 1)) / fromInteger (fib n)

-- Property:  Checks if the ratio between consecutive Fibonacci numbers is close to the golden ratio (phi) within a given tolerance (epsilon).
fibRatioClose :: Integer -> Double -> Property
fibRatioClose n epsilon = n < 2 || abs (fibRatio n - phi) < epsilon ==>
  classify (n < 0) "n < 0" $ 
  classify (n == 0 && n == 1) "n < 2" $ 
  n < 2 || abs (fibRatio n - phi) < epsilon

----------------------------------------------------------------------------------------  3.3  ------------------------------------------------------------------------------

runTests3_3 :: IO ()
runTests3_3 = do
  putStrLn "Testing Fibonacci ratio closeness to φ with varying ε:"
  putStrLn ""
  putStrLn "ε = 1.0 " 
  quickCheck (\n -> fibRatioClose n 1.0)
  putStrLn "ε = 0.1 "
  quickCheck (\n -> fibRatioClose n 0.1)
  putStrLn "ε = 0.01 "
  quickCheck (\n -> fibRatioClose n 0.01)

----------------------------------------------------------------------------------------  3.4  ------------------------------------------------------------------------------
-- Defines a custom generator `arbitraryValidElements` to produce integers between 2 and 52 (inclusive).
arbitraryValidElements :: Gen Integer
arbitraryValidElements = do
  n <- choose (0, 50)  -- Generate a random integer between 0 and 50.
  return (2 + n)  -- Add 2 to shift the range to 2-52.

-- Property: Checks the Fibonacci ratio closeness to phi using the custom generator for 'n'.  This ensures 'n' is always >= 2.
prop_fibRatioCustom :: Double -> Property
prop_fibRatioCustom e = forAll arbitraryValidElements $ \n ->
  fibRatioClose n e 

runTests3_4 :: IO ()
runTests3_4 = do
  putStrLn "Testing Fibonacci ratio closeness to φ with varying ε and n>= 2:"
  putStrLn ""
  putStrLn "ε = 1.0 " 
  quickCheck (prop_fibRatioCustom 1.0)
  putStrLn "ε = 0.1 "
  quickCheck (prop_fibRatioCustom 0.1)
  putStrLn "ε = 0.01 "
  quickCheck (prop_fibRatioCustom 0.01)
  
----------------------------------------------------------------------------------------  3.5 ------------------------------------------------------------------------------
runTests3_5 :: IO ()
runTests3_5 = do
  putStrLn "generate reports of diﬀerent level like in Exercise 2.3"
  putStrLn ""
  putStrLn "ε = 0.01 "
  verboseCheck (prop_fibRatioCustom 0.01)

main :: IO ()
main = do
    putStrLn "-------------  2  -------------"
    putStrLn ""
    runTests2
    putStrLn "-------------  3  -------------"
    putStrLn ""
    runTests3_3
    putStrLn ""
    runTests3_4
    putStrLn ""
    runTests3_5