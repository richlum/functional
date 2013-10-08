-- Questoin 1
-- There are many possible solutions. Here is one. Type signature is omitted.
preparednessQuotient i s c pn pa d n =
                (top pa s c) / (bottom pn d n i)
top pa s c = 8 * (pa * (s + c))
bottom pn d n i = 3 * (pn * (d + n + i))
 
-- Question 2
-- There are many possible solutions. Here is one. Type signature is omitted. 
kungPaoFactor r dm ds n c ft ff s =
                (term1 dm ds n) + (term2 r c ft ff s)
term1 dm ds n = (n / 30) - (ds / dm)
term2 r c ft ff s = (term2top s r) / (term2bottom c ft ff)
term2top s r = 10 * (s ** 2) * (sqrt r)
term2bottom c ft ff = c * (ft - ff + 1)
 
-- Question 3
multiply :: Int -> Int -> Int
multiply n1 n2
	| n1 == 0 = 0
	| otherwise = n2 + (multiply (n1 - 1) n2)
	 
-- -- Here is a bad solution to Question 3
-- multiply :: Int -> Int -> Int
-- multiply n1 n2
-- 	| n1 == 0 = 0
-- 	| n2 == 0 = 0 -- don't test constants as base case!!
-- 	| otherwise = n2 + (multiply (n1 - 1) n2)
 
-- Question 4
multiply_tr :: Int -> Int -> Int
multiply_tr n1 n2 = multiply_tr' n1 n2 0
 
multiply_tr' :: Int -> Int -> Int -> Int
multiply_tr' n1 n2 sum
	| n1 == 0 = sum
	| otherwise = multiply_tr' (n1 - 1) n2 (sum + n2)
	 
-- Question 5 
power :: Int -> Int -> Int
power n1 n2
	| n2 == 0 = 1
	| otherwise = multiply n1 (power n1 (n2 - 1))
	 
-- Question 6 
power_tr :: Int -> Int -> Int
power_tr n1 n2 = power_tr' n1 n2 1
 
power_tr' :: Int -> Int -> Int -> Int
power_tr' n1 n2 product
	| n2 == 0 = product
	| otherwise = power_tr' n1 (n2 - 1) (multiply n1 product)
 
-- Question 7 
harmonic :: Float -> Float
harmonic n
	| n == 1 = 1
	| otherwise = 1/n + harmonic (n - 1)
	 
-- Question 8 
harmonic_tr :: Float -> Float
harmonic_tr n = harmonic_tr' n 1
 
harmonic_tr' :: Float -> Float -> Float
harmonic_tr' n result
	| n == 1 = result
	| otherwise = harmonic_tr' (n - 1) (1/n + result)