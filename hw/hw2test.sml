(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

(*
 * first submission 70/100
 * some details below:
 * 
remove_card: incorrect result, the specified card is in the list once. [IllegalMove exception thrown]

remove_card: incorrect result, the specified card is in the list multiple times. [incorrect answer]

all_same_color: Your function returns an incorrect result when given the list [(Clubs,Ace),(Spades,Ace),(Diamonds,Ace)] [incorrect answer]
officiate: Your function returns an incorrect result when the game should end due to the sum of cards in the player's hand exceeding the goal. [incorrect answer]
officiate: the game ends with no more move and the held-cards are of different colors. [IllegalMove exception thrown]
officiate EXCEPTIONS: Your function does not throw a correct exception when the player attempts to discard card not in their hand. [NO RAISED EXCEPTION]
3a tests failed to run (most likely caused by an incorrect function signature or unimplemented function in the submission)
3b tests failed to run (most likely caused by an incorrect function signature or unimplemented function in the submission)
Used illegal functions in the following problems: ['get_substitutions1', 'similar_names', 'get_substitutions2']

*)


val test1 = all_except_option ("string", ["string"]) = SOME []

val test1_1 = all_except_option ("str", ["string"]) = NONE
							    
val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
									    
val test7_2 = remove_card ([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace)]
									    
val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
								
val test8_2 = all_same_color [(Clubs,Ace),(Spades,Ace),(Diamonds,Ace)] = false
								
val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
             
