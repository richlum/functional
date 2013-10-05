-- Richard Lum
-- 41671785
-- cs312 assign 2
-- Sept 29, 2013
-- ==========================================================

-- note: unit tests included in mytests.h, requires Test.HUnit be loaded to run
--    contains all the tests shown on assignment plus a handful of others
 
-- Main>:l mytests 
-- *Main> runTestTT alltests
-- Cases: 82  Tried: 82  Errors: 0  Failures: 0
-- Counts {cases = 82, tried = 82, errors = 0, failures = 0}

-------------------------------------------------------------   
-- only allowed to use
--   : head tail null elem
-- example

module Assign2
(	myappend,			myappend_pm,
	myremoveduplicates,	myremoveduplicates_pm,
	myintersection,		myintersection_pm,
	mynthtail,			mynthtail_pm,
	mylast, 			mylast_pm,	
	myreverse,			myreverse_pm,
	myreplaceall,		myreplaceall_pm,
	myordered,			myordered_pm
)	where

myappend :: (Eq a) => [a] -> [a] -> [a]
myappend list1 list2
	| list1 == [] 	= list2
	| otherwise	= (head list1) : (myappend (tail list1) list2)

myappend_pm :: (Eq a) =>  [a] -> [a] -> [a]
myappend_pm [] list2 	 = list2
myappend_pm (x:xs) list2 = x:(myappend_pm xs list2)	


-- a2:q1 myremoveduplicates
-- helper function to remove all occurences of supplied elem x from list xs
removeXfrXS::(Eq a) => a -> [a] -> [a]
removeXfrXS x xs
	| not (elem x xs)	= xs
	| head xs == x 		= (removeXfrXS x (tail xs))	
	| otherwise 		= (head xs ) : (removeXfrXS x (tail xs)) 

myremoveduplicates :: (Eq a) => [a] -> [a] 
myremoveduplicates list1  
	| list1 == []		= []
--	| tail list1 == []	= list1  -- redundant, not required
	| not( elem (head list1) (tail list1))	
						= (head list1) : (myremoveduplicates (tail list1))
	| otherwise			= (head list1) : (myremoveduplicates (removeXfrXS (head list1) (tail list1)))


-- helper function that removes all occurences of given char
rmdup_pm :: (Eq a) => a -> [a] -> [a]
rmdup_pm x []           = []
-- rmdup_pm x [y]          = if (x == y) then []  -- redundant
-- 								else [y]
rmdup_pm x (y:ys)		= if (x == y) then (rmdup_pm x ys)
                          else (y:(rmdup_pm x ys))



myremoveduplicates_pm :: (Eq a) => [a] -> [a] 
myremoveduplicates_pm []        = []
--myremoveduplicates_pm [x]       = [x]  -- redundant, not required
myremoveduplicates_pm (x:xs)    = if (not (elem x xs)) then x:(myremoveduplicates_pm xs)
                                  else (x:(myremoveduplicates_pm (rmdup_pm x xs)))
  

-- a2:q2 myintersection
-- note no testing for uniquness in either list input.
myintersection::(Eq a) =>  [a] -> [a] -> [a]
myintersection list1 list2
	| list1 == []                  = []
--	| list2 == []	               = [] -- redundant, not required
	| elem (head list1) list2      = head list1 : (myintersection (tail list1) list2)
	| not (elem(head list1) list2) = myintersection (tail list1) list2


myintersection_pm::(Eq a) =>  [a] -> [a] -> [a]
myintersection_pm [] list2       = []
--myintersection_pm list1 []       = []  -- redundant, not required
myintersection_pm (x:xs) list2   = if (elem x list2) then x:(myintersection_pm xs list2)
                                   else myintersection_pm xs list2

-- a2:q3 mynthtail

mynthtail :: Int -> [a] -> [a]
mynthtail n list
	| null list  = []  -- not redunant, user input empty list
	| n<0        = error "n less than zero"  -- check for user input error
	| n == 0     = list  -- required, not redundant
	| otherwise  = mynthtail (n-1) (tail list)

mynthtail_pm :: Int -> [a] ->  [a]
mynthtail_pm 0 (x)    = x   
mynthtail_pm n []     = []  -- protects user requesting n larger than sequence size
mynthtail_pm n (x:xs) = mynthtail_pm (n-1) xs

-- a2:q4 mylast
mylast ::  [a] -> [a]
mylast list 
	| null list            = []  -- protects against user input null list
	| (null (tail list) )  = [head list]
	| otherwise            = (mylast (tail list))

mylast_pm ::  [a] -> [a]
mylast_pm []		= [] -- protects against user input null list
mylast_pm (x:[])	= [x]
mylast_pm (x:xs)	= mylast_pm xs

-- a2:q5 myreverse

--myreverse :: [a] -> [a]
-- this works but It violates the design restriction to use of head and tail only
myreverse' list
	| null list 		= []
	| null (tail list)	= list
	| otherwise			= last(list) : myreverse' (init list)
-- heres a version that uses parameter outlist to hold reversed list
-- that gets built in reverse order with every call to revlist
-- this allows us to only use the cons operator (a:[a])  and still
-- reverse order   

-- helper function that works down inlist and builds result in outlist of recursive calls 
revlist :: [a] -> [a] -> [a]
revlist inlist outlist
	| null inlist		= outlist
	| otherwise			= revlist ( tail inlist)  ((head inlist):outlist) 

myreverse :: [a] -> [a]
myreverse list
	| null list 		= []
--	| null (tail list)	= list  -- redundant
	| otherwise			= revlist(tail list) [(head list)]

-- helper function that builds up reversed result from right side in second arg 
revlist_pm :: [a] -> [a] -> [a]
revlist_pm [] ys		= ys
revlist_pm (x:xs) ys	= revlist_pm xs (x:ys)
	
myreverse_pm :: [a] -> [a]
myreverse_pm []			= []
--myreverse_pm (x:[])		= [x]  -- redundant
myreverse_pm (x:xs)		= (revlist_pm xs [x])
	

--a2:q6 myreplaceall
-- tested myreplaceall '' 'a' "abacab" -- lexical error, will not accept delete char action
-- not part of stated requirement
myreplaceall:: (Eq a) => a -> a -> [a] -> [a]
myreplaceall ain aout inlist
	|	null inlist				= []
	|	((head inlist) == aout)	= ain:(myreplaceall ain aout (tail inlist))
	|	otherwise 				= (head inlist) : (myreplaceall ain aout (tail inlist))
	
myreplaceall_pm:: (Eq a) => a -> a -> [a] -> [a]
myreplaceall_pm i o []		= []
myreplaceall_pm i o (x:xs)	= if x == o then i:(myreplaceall_pm i o xs)
								else x:(myreplaceall_pm i o xs)

-- a2:q7 myordered 

-- true only if each elem is <= prev elem
myordered:: (Ord a) => [a] -> Bool
myordered list
	| null list 						= True
	| null (tail list)					= True  -- required condition - single element list, not redundant
	| (head list) <= (head (tail list))	= True || myordered (tail list) -- ok char, check next
	| otherwise							= False
	
myordered_pm:: (Ord a) => [a] -> Bool
myordered_pm []		= True
myordered_pm (x:[])	= True  -- not redundant, required condition
myordered_pm (x:xs)	= if (x <= (head xs)) then True||(myordered_pm (tail xs))
						else False
						
