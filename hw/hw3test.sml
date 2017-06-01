(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"
						  
val test4b = longest_string3 ["A","bc","BC"] = "bc"
						   
val test4c = longest_string4 ["A","B","C"] = "C"
						 
val test5 = longest_capitalized ["A","bc","C"] = "A"
						     
val test5b = longest_capitalized ["A","bc","CD"] = "CD"
						       
val test5c = longest_capitalized ["Ab","bc","Cd"] = "Ab"

val test5d = longest_capitalized ["ab","bc","cd"] = ""

val test6 = rev_string "abc" = "cba"

val test6b = rev_string "xyz" = "zyx"
				    
val test6c = rev_string "" = ""
				
val test6d = rev_string "A" = "A"

val test6e = rev_string2 "abc" = "cba"

val test6f = rev_string2 "xyz" = "zyx"
				    
val test6g = rev_string2 "" = ""
				
val test6h = rev_string2 "A" = "A"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
										
val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE


val test8b = all_answers (fn x => if x = 3 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [3]
											 
val test8c = all_answers (fn x => if x > 2 andalso x < 6 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [3, 4, 5]
															     										       
val test9a = count_wildcards Wildcard = 1


val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

