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
prop_Equalff' :: Integer -> Bool
prop_Equalff' n = n < 2 || f n == f' n
----------------------------------------------------------------------------------------  2.3  ------------------------------------------------------------------------------
runTests2 :: IO ()
runTests2 = do
  putStrLn "f(n) == f'(n) :"
  quickCheck prop_Equalff'
  verboseCheck prop_Equalff'





main :: IO ()
main = do
    putStrLn "-------------  2  -------------"
    putStrLn ""
    runTests2