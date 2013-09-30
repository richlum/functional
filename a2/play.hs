--myelem item [] = False
--myelem item (y:ys) = item == y || myelem item ys

--  illegal reuse of same var twice in pattern match
--myelem' item [] = False
--myelem' item (item:ys) = True
--myelem' item (_:ys) = myelem item ys

myelem item inlist
	| null inlist = False
	| item == head inlist = True
	| otherwise = myelem item (tail inlist)


myl  ::  [Int] -> [Int]
myl inlist
	| null inlist = []
	| null (tail inlist) = [head inlist]
	| otherwise = myl (tail inlist)
