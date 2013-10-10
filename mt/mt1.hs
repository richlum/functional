--1

geom::(Floating a) =>  a-> a->a->a
geom a r n
	| n == 1 	= a
	| otherwise	= (a * r ** (n-1)) + (geom a r (n-1) )
geom_tr :: (Eq a,Floating a) =>  a -> a -> a -> a
geom_tr a r n
	| r == 1	= a
	| otherwise 	= geom_h a r n 0

geom_h :: (Floating a) => a -> a-> a-> a -> a
geom_h a r n accum
	| n == 0 	= accum
	| otherwise 	= geom_h a r (n-1) ((a * r**(n-1)) + accum)  

--2  copy paste from question sheet

--3 

(+!)::[a] -> [a] ->  [a]
(+!) [] ys	= ys
(+!) (x:xs) ys = x:xs +! ys

--4
--5
repl :: (Eq a) => a -> a -> [a] -> [a]
repl x y list
	| null list	= []
	| head list == x	= y:(repl x y (tail list))
	| head list == y	= x:(repl x y (tail list))
	| otherwise		= (head list):(repl x y (tail list))

repl_pm :: (Eq a) =>  a -> a->[a]->[a]
repl_pm _ _ []		= []
repl_pm x y (z:zs)	= if x == z then
				y:(repl_pm x y zs)
			  else
				if y == z then
					x:(repl_pm x y zs)
				else
					z:(repl_pm x y zs)




--6
--sc sc :: (Eq a) => [(a, [Char])] -> a -> [Char]
-- 
--sc:: (Eq a) => [(Char,Char)] -> a -> Char
sc [] _  	= '?'
sc key x 	
	| fst(head key) == x	= snd(head key)
	| otherwise		= sc (tail key) x 

--crypt:: [(a,a)] -> [a] -> [a]
crypt key plain
	| null plain 	= []
	| otherwise	= sc key (head plain):(crypt key (tail plain))

keystone = zip ['a','b' .. 'z'] ['z','y' .. 'a']
--crypt key "rolevszhpvoo"

