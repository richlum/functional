module Assign2sol
(	
	myremoveduplicates,	myremoveduplicates_pm,
	myintersection,		myintersection_pm,
	mynthtail,			mynthtail_pm,
	mylast, 			mylast_pm,	
	myreverse,			myreverse_pm,
	myreplaceall,		myreplaceall_pm,
	myordered,			myordered_pm
)	where
   
   -- Problem 1

myremoveduplicates list1
	| null list1 = []
	| elem (head list1) (tail list1) = myremoveduplicates (tail list1)
	| otherwise = (head list1):(myremoveduplicates (tail list1))

 

myremoveduplicates_pm [] = []
myremoveduplicates_pm (x:xs)
	| elem x xs = myremoveduplicates_pm xs
	| otherwise = x:(myremoveduplicates_pm xs)

 

-- Problem 2

myintersection list1 list2
	| null list1 = []
	| elem (head list1) list2 = (head list1):(myintersection (tail list1) list2)
	| otherwise = myintersection (tail list1) list2

 

myintersection_pm [] list2 = []
myintersection_pm (x:xs) list2
	| elem x list2 = x:(myintersection_pm xs list2)
	| otherwise = myintersection_pm xs list2

 

-- Problem 3

-- This is a solution which implements the least features specified in the problem. It would be great if you check the length of the list first to guarantee that the list is not empty.

mynthtail num list1
	| num == 0 = list1
	| otherwise = mynthtail (num - 1) (tail list1)

	 

mynthtail_pm 0 list1 = list1
mynthtail_pm n (x:xs) = mynthtail_pm (n - 1) xs

 

-- Problem 4

mylast list1
	| null list1 = []
	| null (tail list1) = list1
	| otherwise = mylast (tail list1)

 

mylast_pm [] = []
mylast_pm [x] = [x]
mylast_pm (_:xs) = mylast_pm xs

 

-- Problem 5

myreverse list1 = myreverse' list1 []
myreverse' list1 result
	| null list1 = result
	| otherwise = myreverse' (tail list1) ((head list1):result)

 

myreverse_pm list1 = myreverse_pm' list1 []
myreverse_pm' [] result = result
myreverse_pm' (x:xs) result = myreverse_pm' xs (x:result)

 

-- Problem 6

myreplaceall item1 item2 list1
	| null list1 = []
	| item2 == head list1 = item1:(myreplaceall item1 item2 (tail list1))
	| otherwise = (head list1):(myreplaceall item1 item2 (tail list1))

 

myreplaceall_pm item1 item2 [] = []
myreplaceall_pm item1 item2 (x:xs)
	| item2 == x = item1:(myreplaceall_pm item1 item2 xs)
	| otherwise = x:(myreplaceall_pm item1 item2 xs)

 

-- Problem 7

myordered list1
	| null list1 = True
	| null (tail list1) = True
	| (head list1) <= (head (tail list1)) = myordered (tail list1)
	| otherwise = False

 

myordered_pm [] = True
myordered_pm [x] = True
myordered_pm (x1:(x2:xs))
	| x1 <= x2 = myordered_pm (x2:xs)
	| otherwise = False
