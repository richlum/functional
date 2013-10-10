-- This file is only the Unit tests for the assignment
-- See the other file assign2.hs for the solution code

-- richard lum   41671685
-- cs312 assignment 2  
   
-- must have HUnit modules loaded
-- to run from ghci, load this file    :l mytests
-- Main> runTestTT mytests
-- Main> runTestTT atests



import Test.HUnit
import Assign2sol



remduplc  = TestCase (assertEqual "for:myremoveduplicates \"abacad\"" ("abcd")  (myremoveduplicates "abacad")) 
remduplc1 = TestCase (assertEqual "remduplc1" ("the quickbrownfxjmpdvlazyg")  (myremoveduplicates "the quick brown fox jumped over the lazy dog")) 
remdupln  = TestCase (assertEqual "for:myremoveduplicates [3,2,1,3,2,2,1,1]" ([3,2,1])  (myremoveduplicates [3,2,1,3,2,2,1,1])) 
remdupln1 = TestCase (assertEqual  "remdupln1" ([6,4,5,1,2,3])  (myremoveduplicates [6,4,5,1,2,3,3,2,1,4,3,2,2,1,1,3,2,1])) 
remdupln3 = TestCase (assertEqual  "remdupln3" ([1])  (myremoveduplicates [1])) 
remdupln2 = TestCase (assertEqual  "remdupln2" ([])  (myremoveduplicates []::[Int])) 


remduplcpm  = TestCase (assertEqual "for:myremoveduplicates_pm \"abacad\"" ("abcd")  (myremoveduplicates_pm "abacad")) 
remduplcpm1 = TestCase (assertEqual "remduplcpm1" ("the quickbrownfxjmpdvlazyg")  (myremoveduplicates_pm "the quick brown fox jumped over the lazy dog")) 
remduplnpm  = TestCase (assertEqual "for:myremoveduplicates_pm [3,2,1,3,2,2,1,1]" ([3,2,1])  (myremoveduplicates_pm [3,2,1,3,2,2,1,1])) 
remduplnpm1 = TestCase (assertEqual  "remduplnpm1" ([6,4,5,1,2,3])  (myremoveduplicates_pm [6,4,5,1,2,3,3,2,1,4,3,2,2,1,1,3,2,1])) 
remduplnpm2 = TestCase (assertEqual  "remduplnpm1" ([])  (myremoveduplicates_pm []::[Int])) 
remduplnpm3 = TestCase (assertEqual  "remduplnpm3" ([1])  (myremoveduplicates_pm [1])) 

atests = TestList[
	remduplc, remduplc1, remdupln,remdupln1,remdupln2,remdupln3
	,remduplcpm,remduplnpm,remduplcpm1,remduplnpm1,remduplnpm2
	]


q21 = TestCase (assertEqual "q21" "bc"    ( myintersection "abc" "bcd"))
q22 = TestCase (assertEqual "q22" [4,2,1] ( myintersection [3,4,2,1] [5,4,1,6,2] ))
q23 = TestCase (assertEqual "q23" []      ( myintersection [] [1,2,3] ))
q24 = TestCase (assertEqual "q24" ""      ( myintersection "abc" ""))
q25 = TestCase (assertEqual "q25" [2,3,5] ( myintersection [1..10] [2,22,33,5,99,3]))
q26 = TestCase (assertEqual "q26" "hoc"   ( myintersection "holycw" "batmanandrobinhaveicecream"))


q21b = TestCase (assertEqual "q21b" "bc"    ( myintersection_pm "abc" "bcd"))
q22b = TestCase (assertEqual "q22b" [4,2,1] ( myintersection_pm [3,4,2,1] [5,4,1,6,2] ))
q23b = TestCase (assertEqual "q23b" []      ( myintersection_pm [] [1,2,3] ))
q24b = TestCase (assertEqual "q24b" ""      ( myintersection_pm "abc" ""))
q25b = TestCase (assertEqual "q25b" [2,3,5] ( myintersection_pm [1..10] [2,22,33,5,99,3]))
q26b = TestCase (assertEqual "q26b" "hoc"   ( myintersection_pm "holycw" "batmanandrobinhaveicecream"))


q2tests = TestList[ q21,q22,q23,q24,q25,q26, 
		 q21b,q22b,q23b,q24b,q25b,q26b 
 ]


q31  = TestCase (assertEqual "q31b"  "abcd" ( mynthtail       0 "abcd"       )) 
q32  = TestCase (assertEqual "q32b"  "bcd"  ( mynthtail       1 "abcd"       )) 
q33  = TestCase (assertEqual "q33b"  "cd"   ( mynthtail       2 "abcd"       )) 
q34  = TestCase (assertEqual "q34b"  "d"    ( mynthtail       3 "abcd"       ))  
q35  = TestCase (assertEqual "q35b"  ""     ( mynthtail       4 "abcd"       )) 
q36  = TestCase (assertEqual "q36b"  [3,4]  ( mynthtail       2 [1, 2, 3, 4] ))  
q37  = TestCase (assertEqual "q37b"  []     ( mynthtail       4 [1, 2, 3, 4] ))
q38  = TestCase (assertEqual "q37b"  []     ( mynthtail       5 [1, 2, 3, 4] ))
 
q31b = TestCase (assertEqual "q31b"  "abcd" ( mynthtail_pm    0 "abcd"       )) 
q32b = TestCase (assertEqual "q32b"  "bcd"  ( mynthtail_pm    1 "abcd"       )) 
q33b = TestCase (assertEqual "q33b"  "cd"   ( mynthtail_pm    2 "abcd"       )) 
q34b = TestCase (assertEqual "q34b"  "d"    ( mynthtail_pm    3 "abcd"       ))  
q35b = TestCase (assertEqual "q35b"  ""     ( mynthtail_pm    4 "abcd"       )) 
q36b = TestCase (assertEqual "q36b"  [3,4]  ( mynthtail_pm    2 [1, 2, 3, 4] ))  
q37b = TestCase (assertEqual "q37b"  []     ( mynthtail_pm    4 [1, 2, 3, 4] ))
q38b = TestCase (assertEqual "q37b"  []     ( mynthtail_pm    5 [1, 2, 3, 4] ))
  
q3tests = TestList[ q31,q32,q33,q34,q35,q36, q37,q38,
		 q31b,q32b,q33b,q34b,q35b,q36b,q37b,q38b 
 ]

 
q41  = TestCase (assertEqual "q41 " ""    ( mylast     ""            ))
q42  = TestCase (assertEqual "q42 " "b"   ( mylast     "b"           ))
q43  = TestCase (assertEqual "q43 " "d"   ( mylast     "abcd"        ))
q44  = TestCase (assertEqual "q44 " [4]   ( mylast     [1, 2, 3, 4]  ))
-- keep the compiler happy with empty list type specifier
q45  = TestCase (assertEqual "q45 " ([]::[Int])    ( mylast     []            ))
q46  = TestCase (assertEqual "q46 " [100] ( mylast     [1..100]      ))
q47  = TestCase (assertEqual "q47 " [-50] ( mylast    [100,99..(-50)]))
    
 
q41b = TestCase (assertEqual "q41b" ""    ( mylast_pm ""            ))
q42b = TestCase (assertEqual "q42b" "b"   ( mylast_pm "b"           ))
q43b = TestCase (assertEqual "q43b" "d"   ( mylast_pm "abcd"        ))
q44b = TestCase (assertEqual "q44b" [4]   ( mylast_pm [1, 2, 3, 4]  ))
q45b = TestCase (assertEqual "q45b" ([]::[Int])   ( mylast_pm []            ))
q46b = TestCase (assertEqual "q46b" [100] ( mylast_pm [1..100]      ))
q47b = TestCase (assertEqual "q47b" [-50] ( mylast_pm[100,99..(-50)]))

q4tests = TestList[ q41,q42,q43,q44,q45,q46, 
		 q41b,q42b,q43b,q44b,q45b,q46b 
 ]

q51 = TestCase (assertEqual "q51"  ""       ( myreverse "" 			 ))
q52 = TestCase (assertEqual "q52"  "cba"    ( myreverse "abc" 		 ))
q53 = TestCase (assertEqual "q53"  [3, 2, 1]( myreverse [1, 2, 3] 	 ))
q54 = TestCase (assertEqual "q54"  ([]::[Int])( myreverse [] 			 ))
 
q51b = TestCase (assertEqual "q51b"  ""       ( myreverse_pm "" 			 ))
q52b = TestCase (assertEqual "q52b"  "cba"    ( myreverse_pm "abc" 		 ))
q53b = TestCase (assertEqual "q53b"  [3, 2, 1]( myreverse_pm [1, 2, 3] 	 ))
q54b = TestCase (assertEqual "q54b"  ([]::[Int]) ( myreverse_pm [] 			 ))
                                     
q5tests = TestList[ q51, q52, q53, q54, 
					q51b, q52b, q53b, q54b ]
					
					
q61	 = TestCase (assertEqual "q61"  [3,0,3,1,3,2,3]  ( myreplaceall 3 7 [7,0,7,1,7,2,7] ))	
q62	 = TestCase (assertEqual "q62"  ""				 ( myreplaceall 'x' 'a' "" 			))  
q63	 = TestCase (assertEqual "q63"  "xbxcxd"		 ( myreplaceall 'x' 'a' "abacad" 	))	
q63c = TestCase (assertEqual "q63c"  "nbibabcb"		 ( myreplaceall 'b' ' ' "n i a c " 	))
q61b = TestCase (assertEqual "q61b"  [3,0,3,1,3,2,3] ( myreplaceall_pm 3 7 [7,0,7,1,7,2,7] ))	
q62b = TestCase (assertEqual "q62b"  ""				 ( myreplaceall_pm 'x' 'a' "" 			))  
q63b = TestCase (assertEqual "q63b"  "xbxcxd"		 ( myreplaceall_pm 'x' 'a' "abacad" 	))	  
q64  = TestCase (assertEqual "q64"   "mixxixxippi"	 ( myreplaceall_pm 'x' 's' "mississippi" 	))
q64b = TestCase (assertEqual "q64b"  "nuisance"		 ( myreplaceall_pm 'x' 'b' "nuisance" 	))
q64c = TestCase (assertEqual "q64c"  "nbibabcb"		 ( myreplaceall_pm 'b' ' ' "n i a c " 	))


q6tests = TestList[ q61, q62, q63, q64, q63c,q64c,
					q61b, q62b, q63b, q64b,q64c ]
                                   
									 
q71  = TestCase (assertEqual "q71" True	  ( myordered ([]::[Int])))
q72  = TestCase (assertEqual "q72" True	  ( myordered [1] 		))
q73  = TestCase (assertEqual "q73" True	  ( myordered [1,2]		))
q74  = TestCase (assertEqual "q74" True	  ( myordered [1,1] 	))
q75  = TestCase (assertEqual "q75" False  ( myordered [2,1] 	))
q76  = TestCase (assertEqual "q76" True	  ( myordered "abcdefg" ))
q77  = TestCase (assertEqual "q77" False  ( myordered "ba" 		))
q78  = TestCase (assertEqual "q78" True ( myordered "a" 	))
q79  = TestCase (assertEqual "q79" True ( myordered "" 	))
q71b = TestCase (assertEqual "q71b" True  ( myordered_pm ([]::[Int])))
q72b = TestCase (assertEqual "q72b" True  ( myordered_pm [1] 	))
q73b = TestCase (assertEqual "q73b" True  ( myordered_pm [1,2]	))
q74b = TestCase (assertEqual "q74b" True  ( myordered_pm [1,1] 	))
q75b = TestCase (assertEqual "q75b" False ( myordered_pm [2,1] 	))
q76b = TestCase (assertEqual "q76b" True  ( myordered_pm "abcdefg" ))
q77b = TestCase (assertEqual "q77b" False ( myordered_pm "ba" 	))
q78b = TestCase (assertEqual "q78b" True ( myordered_pm "a" 	))
q79b = TestCase (assertEqual "q79b" True ( myordered_pm "" 	))

q7tests = TestList [ q71, q72, q73, q74,q75,q76,q77,q78,q79, 
					q71b, q72b, q73b, q74b,q75b,q76b,q77b,q78b,q79b ]
 

alltests = TestList [  atests, q2tests, q3tests, q4tests,
					q5tests, q6tests, q7tests]
