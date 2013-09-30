myelem item [] = False
myelem item (y:ys) = item == y || myelem item ys

--  illegal reuse of same var twice in pattern match
--myelem' item [] = False
--myelem' item (item:ys) = True
--myelem' item (_:ys) = myelem item ys

