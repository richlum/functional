-- a1, q1

--pq = 8*p_a*(S+C) / (3*p_n*(D+N+I))
pquotient :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
pquotient prep_hrs sleep_hrs cons_stims req_hours diff nerves importance = 
    (pqnumerator prep_hrs sleep_hrs cons_stims)/ 
    (pqdenominator req_hours diff nerves importance )

pqnumerator :: Float ->  Float -> Float -> Float
pqnumerator prep_hrs sleep_hrs cons_stims = 
    8 * prep_hrs * (prepMultipliers sleep_hrs cons_stims) 

prepMultipliers :: Float -> Float -> Float
prepMultipliers sleep_hrs cons_stims  = sleep_hrs + cons_stims

pqdenominator :: Float -> Float -> Float -> Float -> Float
pqdenominator req_hours diff nerves importance = 
    3 * req_hours * (diffMultipliers diff nerves importance)

diffMultipliers :: Float -> Float -> Float -> Float
diffMultipliers diff nerves importance = diff + nerves + importance





