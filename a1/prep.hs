-- a1, q1
--pq =  8*pa*(s+c)/(3*pn*(d+n+i))
-- preparednessQuotient i s c pn pa d n
-- preparednessQuotient 2 2 2 2  2  2 2 = 1.78
preparednessQuotient :: Float -> Float -> Float -> Float -> 
	Float -> Float -> Float -> Float
preparednessQuotient importance sleep_hrs cons_stims prep_needed prep_actual diff nerves = 
    (pqnumerator prep_actual sleep_hrs cons_stims)/ 
    (pqdenominator prep_needed diff nerves importance )

pqnumerator :: Float ->  Float -> Float -> Float
pqnumerator prep_actual sleep_hrs cons_stims = 
    8 * prep_actual * (prepMultipliers sleep_hrs cons_stims) 

prepMultipliers :: Float -> Float -> Float
prepMultipliers sleep_hrs cons_stims  = sleep_hrs + cons_stims

pqdenominator :: Float -> Float -> Float -> Float -> Float
pqdenominator prep_needed diff nerves importance = 
    3 * prep_needed * (diffMultipliers diff nerves importance)

diffMultipliers :: Float -> Float -> Float -> Float
diffMultipliers diff nerves importance = diff + nerves + importance

-- a1, q2
-- K = (n/30 - ds/dm) + ( 10*S^2*sqrt(r))/(c*(ft-ff+1))
--kungPaoFactor r dm ds n  c  ft ff s
--kungPaoFactor 1 40 20 30 10 32 1  4 = 1.0
kungPaoFactor :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
kungPaoFactor recog dollarMonth dollarSpent numDate currBP foodTot foodFur stress =
  (kpLterm numDate dollarSpent dollarMonth) + (kpRterm stress recog currBP foodTot foodFur)
  
kpLterm:: Float -> Float -> Float -> Float
kpLterm numDate dollarSpent dollarMonth = 
  (monthfrac numDate ) - (budget dollarSpent dollarMonth)

monthfrac:: Float -> Float
monthfrac numDate = numDate / 30

budget :: Float -> Float -> Float
budget dollarSpent dollarMonth = dollarSpent / dollarMonth

kpRterm:: Float -> Float -> Float -> Float -> Float -> Float
kpRterm stress recog currBP foodTot foodFur = 
  (kprNumerator stress recog) / (kprDenominator currBP foodTot foodFur)
  
kprNumerator :: Float -> Float -> Float
kprNumerator stress recog = 10 * (stress^2) * sqrt(recog)

kprDenominator :: Float -> Float -> Float -> Float
kprDenominator currBP foodTot foodFur = currBP * (foodTot -foodFur + 1) 

-- a1, q3
multiply:: Integer -> Integer -> Integer
multiply 0 _ = 0
multiply _ 0 = 0
multiply 1 y = y
multiply x 1 = x
multiply x y = y + (multiply (x-1) y )
-- alternative using guards
gmult:: Integer -> Integer -> Integer
gmult x y 
	| x == 0  = 0
	| y == 0  = 0
	| x == 1  = y
	| y == 1  = x
	| otherwise = x + gmult (y-1) x


-- a1, q4
multiply_tr:: Integer -> Integer -> Integer
multiply_tr x y = mult x y 0

mult:: Integer -> Integer ->  Integer ->  Integer
mult x y accumulator 
	| x == 0 	= 0
	| y == 0 	= 0
	| x == 1 	= accumulator + y
	| y == 1 	= accumulator + x
	| otherwise 	= mult (x-1) y (accumulator+y)


-- a1, q5
power :: Integer -> Integer -> Integer
power x y  -- x^y
	| x == 0	= 0
	| x == 1	= 1
	| y == 0	= 1
	| y == 1	= x
	| otherwise 	= multiply x  (power x (y-1)) 	



