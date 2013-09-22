-- a1, q1
--pq =  8*pa*(s+c)/(3*pn*(d+n+i))
-- preparednessQuotient i s c pn pa d n
-- preparednessQuotient 2 2 2 2  2  2 2 = 1.78
preparednessQuotient :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
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



