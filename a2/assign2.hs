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
	myremoveduplicates_pm,
	myintersection,
	myintersection_pm
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



rmdup_pm :: (Eq a) => a -> [a] -> [a]
rmdup_pm x []           = []
rmdup_pm x [y]          = if (x == y) then []
                          else [y]
rmdup_pm x (y:ys)	= if (x == y) then (rmdup_pm x ys)
                          else (y:(rmdup_pm x ys))



myremoveduplicates_pm :: (Eq a) => [a] -> [a] 
myremoveduplicates_pm []        = []
myremoveduplicates_pm [x]       = [x]
myremoveduplicates_pm (x:xs)    = if (not (elem x xs)) then x:(myremoveduplicates_pm xs)
                                  else (x:(myremoveduplicates_pm (rmdup_pm x xs)))
  

-- q2 myintersection
-- note no testing for uniquness in either list input.
myintersection::(Eq a) =>  [a] -> [a] -> [a]
myintersection list1 list2
	| list1 == []                  = []
	| list2 == []	               = []
	| elem (head list1) list2      = head list1 : (myintersection (tail list1) list2)
	| not (elem(head list1) list2) = myintersection (tail list1) list2


myintersection_pm::(Eq a) =>  [a] -> [a] -> [a]
myintersection_pm [] list2       = []
myintersection_pm list1 []       = []
myintersection_pm (x:xs) list2   = if (elem x list2) then x:(myintersection_pm xs list2)
                                   else myintersection_pm xs list2



