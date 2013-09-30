-- must have HUnit modules loaded
-- to run from ghci, load this file    :l mytests
-- Main> runTestTT mytests
-- Main> runTestTT atests



import Test.HUnit
import Assign2

test01 = TestCase (assertEqual "for: myappend [1..10] [21..30]" ([1..10] ++ [21..30]) (myappend [1..10] [21..30]))
test02 = TestCase (assertEqual "for: myappend_pm [1..10] [21..30]" ([1..10] ++ [21..30]) (myappend_pm [1..10] [21..30]))
test03 = TestCase (assertEqual "for: myappend [1..5] [21..30]" ([1..5] ++ [21..30]) (myappend [1..5] [21..30]))
test04 = TestCase (assertEqual "for: myappend_pm [1..5] [21..30]" ([1..5] ++ [21..30]) (myappend_pm [1..5] [21..30]))
test05 = TestCase (assertEqual "for: myappend [] [21..30]" ([] ++ [21..30]) (myappend [] [21..30]))
test06 = TestCase (assertEqual "for: myappend_pm [] [21..30]" ([] ++ [21..30]) (myappend_pm [] [21..30]))
test07 = TestCase (assertEqual "for: myappend [1..5] []" ([1..5] ++ []) (myappend [1..5] []))
test08 = TestCase (assertEqual "for: myappend_pm [1..5] []" ([1..5] ++ []) (myappend_pm [1..5] []))
 
egtests = TestList [ 
	test01, test02 , test03, test04
 	,test05, test06 , test07, test08
	]

remduplc = TestCase (assertEqual "for:myremoveduplicates \"abacad\"" ("abcd")  (myremoveduplicates "abacad")) 
remduplc1 = TestCase (assertEqual "remduplc1" ("the quickbrownfxjmpdvlazyg")  (myremoveduplicates "the quick brown fox jumped over the lazy dog")) 
remdupln = TestCase (assertEqual "for:myremoveduplicates [3,2,1,3,2,2,1,1]" ([3,2,1])  (myremoveduplicates [3,2,1,3,2,2,1,1])) 
remdupln1 = TestCase (assertEqual  "remdupln1" ([6,4,5,1,2,3])  (myremoveduplicates [6,4,5,1,2,3,3,2,1,4,3,2,2,1,1,3,2,1])) 

remduplcpm = TestCase (assertEqual "for:myremoveduplicates_pm \"abacad\"" ("abcd")  (myremoveduplicates_pm "abacad")) 
remduplcpm1 = TestCase (assertEqual "remduplcpm1" ("the quickbrownfxjmpdvlazyg")  (myremoveduplicates_pm "the quick brown fox jumped over the lazy dog")) 
remduplnpm = TestCase (assertEqual "for:myremoveduplicates_pm [3,2,1,3,2,2,1,1]" ([3,2,1])  (myremoveduplicates_pm [3,2,1,3,2,2,1,1])) 
remduplnpm1 = TestCase (assertEqual  "remduplnpm1" ([6,4,5,1,2,3])  (myremoveduplicates_pm [6,4,5,1,2,3,3,2,1,4,3,2,2,1,1,3,2,1])) 


atests = TestList[
	remduplc, remduplc1, remdupln,remdupln1
	,remduplcpm,remduplnpm,remduplcpm1,remduplnpm1
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

