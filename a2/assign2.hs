-- Richard Lum
-- 41671785
-- cs312 assign 2
-- Sept 29, 2013
-- ==========================================================
-- only allowed to use
--   : head tail null elem
-- example

module Assign2
(	myappend,
	myappend_pm,
	myremoveduplicates,
	myremoveduplicates_pm
)	where

myappend :: (Eq a) => [a] -> [a] -> [a]
myappend list1 list2
	| list1 == [] 	= list2
	| otherwise	= (head list1) : (myappend (tail list1) list2)

myappend_pm :: (Eq a) =>  [a] -> [a] -> [a]
myappend_pm [] list2 	 = list2
myappend_pm (x:xs) list2 = x:(myappend_pm xs list2)	


-- a2 q1 myremoveduplicates
-- helper function to remove all occurences of supplied elem x from list xs
removeXfrXS::(Eq a) => a -> [a] -> [a]
removeXfrXS x xs
	| not (elem x xs)	= xs
	| head xs == x 		= (removeXfrXS x (tail xs))	
	| otherwise 		= (head xs ) : (removeXfrXS x (tail xs)) 

myremoveduplicates :: (Eq a) => [a] -> [a] 
myremoveduplicates list1  
	| list1 == []				= []
	| tail list1 == []			= list1
	| not( elem (head list1) (tail list1))	= (head list1) : (myremoveduplicates (tail list1))
	| otherwise				= (head list1) : (myremoveduplicates (removeXfrXS (head list1) (tail list1)))

myremoveduplicates_pm :: (Eq a) => [a] -> [a] 
myremoveduplicates_pm list1  
	| list1 == []				= []
	| tail list1 == []			= list1
	| not( elem (head list1) (tail list1))	= (head list1) : (myremoveduplicates (tail list1))
	| otherwise				= (head list1) : (myremoveduplicates (removeXfrXS (head list1) (tail list1)))



