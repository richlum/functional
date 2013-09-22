happy(vincent).
listens2Music(butch).
playsAirGuitar(vincent):-
	listens2Music(vincent),  /* AND = conjunction */
	happy(vincent).
/* disjunction, multiple clauses
playsAirGuitar(butch):-  
	happy(butch).
playsAirGuitar(butch):-
	listens2Music(butch).
*/
playsAirGuitar(butch):-
	happy(butch);   /* OR = disjunction */
	listens2Music(butch).

/* 
eg:
? playsAirGuitar(vincent).  False
? playsAirGuitar(butch). True
*/


