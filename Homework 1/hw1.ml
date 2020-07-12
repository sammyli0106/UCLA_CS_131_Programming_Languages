(* Provided from TA : type definition *)
(* Setup 1 : create a tuple that store non-terminal token on the left hand side and terminal token on the right hand side *)
(* N = nonterminal symbol and T = terminal symbol *)
type ('nonterminal, 'terminal) symbol = 
	| N of 'nonterminal
	| T of 'terminal 

(* Helper Function & set up section *)
(* Helper function 1 : function that iterate through the list and check whether an element is in the list *)
(* First parameter : a list *)
(* Second paramter : element to be matched *)
let rec check_list input_list element =
	match input_list with 
	| head_pos :: tail_pos ->
	(* First, check if the head element equal to the search element, if yes, return true *)
	if (head_pos = element) then true
	(* If not, recursively search the element by passing the rest of the list and checking element *)
	else (check_list tail_pos element)
	(* Second, I need to check if the list is empty, if it is, return false *)
	| [] -> false;; 

(* Helper function 2 : Extract all the left nodes and organize them into a list *)
(* Sources : https://stackoverflow.com/questions/26005545/ocaml-extract-nth-element-of-a-tuple *)
let extract_left_tuple (leftTuple, _) = leftTuple;;

(* Helper function 3 : Extract all the right nodes and organize them into a list *)
(* Sources : https://stackoverflow.com/questions/26005545/ocaml-extract-nth-element-of-a-tuple *)
let extract_right_tuple (_, rightTuple) = rightTuple;;

(* Task 1 : function subset a b return true if the set represented by the list a is a subset of the set represented by the list b *)
let rec subset a b =
	match a with
	| head_pos :: tail_pos ->
	(* First, we will check whether the head element of list a is inside the list b, if yes, then recursively search 
	through the rest of the list a and check for matching until it is empty *)
	(check_list b head_pos) && (subset tail_pos b)
	(* Second, empty list is considered as subset of list b *)
	| [] -> true;;

(* Task 2 : function equal_sets a b that return true if and only if the represented sets are equal *)
(* Sets are equal if they are subset of each other *)
let equal_sets a b = (subset a b) && (subset b a);;

(* Task 3 : function set_union a b that returns a list represented the union of the two list a and b *)
let rec set_union a b = 
	match a with 
	(* First, check if head element of set a is inside set b, if yes, then recursively
	search through the rest of set a *)
	| head_pos :: tail_pos -> if (check_list b head_pos) then (set_union tail_pos b)
	(* The @ operator is concatenate two list together *)
	(* If head element is not inside set b, then concatenate the head element to set b, recursively search again *)
							else (set_union tail_pos ([head_pos] @ b))
	(* If a is an empty set, then union of set a and b is just set b *)
	| [] -> b;;

(* Task 4 : function set_intersection a b that returns a list represented the intersection of the two list a and b *)
(* We keep on adding the head element to the list when we recursively search through the rest of the elements*)
(* We only insert the element into the list when they are the same and exclude it when they are different *)
let set_intersection a b  = List.filter (fun x -> List.mem x a) b;;

(* Task 5 : function set_diff a b that returns a list representing a-b which is the set of all members of a that are not members of b *)
(* Similar to the one above, but opposite, we are finding difference here, flip the two cases from above for the if else statement *)
let set_diff a b = List.filter (fun x -> not (List.mem x b)) a;;

(* Task 6 : function computed_fixed_point eq f x that return the computed fixed point for f with respect to x *)
let rec computed_fixed_point eq f x =
	(* First, we need to check if the function f(x) is equal to x, if yes then x *)
	if (eq x (f x))
	then x 
	(* If not, then we need to keep on checking whether f (f (f x)) etc is equal to x recursively *)
	else (computed_fixed_point eq f (f x));;

(* Task 7 : function filter_reachable g that return a copy of the grammar g with all unreachable rules removed *)
(* It need to preserve the order of rules and all returned rules should be in the same order as in the rules in the grammar *)

(* Helper function 6 : Create a function that will extract all the non-terminal symbols that are reachable *)
(* We are gonna recursively search level by level to check all the non-terminal symbols *)
let rec extract_reachable current_symbols filter_rules = 
	(* Helper function 5 : Create a function that will extract all the available non-terminal symbols *)
	let rec extract_symbols current_symbols filter_rules =
		match filter_rules with 
		(* If the given grammar rules are empty, then non-terminal symbols are input symbols *)
		| [] -> current_symbols
		(* Separate into temporary rules and the rest of the rules *)
		| head_pos :: tail_pos ->
			(* We are checking whether we have any non-terminal symbols we have not handled *)
			(* We do not do anything for the non-terminal symbols we have already handled *)
			(* First, get the left hand side element *)
			let left_elem = extract_left_tuple head_pos in
			(* Check if the element is already inside our current symbol list *)
			let current = check_list current_symbols left_elem in 
			(* If temporary symbols in reachable symbols *)
			if (current)
			then 
				(* If yes, then we get the righ hand side element *)
				let right_elem = extract_right_tuple head_pos in 

				(* Helper function 4 : From the right side of the expression, search and find the non-terminal elements *)
				(* This will return a list of non-terminal symbols, include both reachable and unreachable *)
				let rec find_non_terminal rightElem = 
					match rightElem with 
					(* If the right hand side of any rule is empty, then there are no non-terminal elements *)
					(* Just return an empty list *)
					| [] -> []
					(* If the first element from right hand side has the type terminal, 
					then we will skip the element and recursively search the rest of the list *)
					| T _ :: back ->
					(find_non_terminal back)
					(* Check if the first element from right hand side has the type non-terminal *)
					(* If yes, then add the head element to the output list and recursively serach the rest of the list to 
					look for non-terminal symbols *)
					| N front :: back ->
					front :: (find_non_terminal back)
				in

				(* Find out the non-terminal symbols from right hand side 
				that are not in the current symbol list yet *)
				let check_next_elem = find_non_terminal right_elem in 
				(* Use the union function to add the newly founded element *)
				let union = set_union current_symbols check_next_elem in 
				(* Recurisvely search by passing the updated symbol list and the rest of the list *)
				extract_symbols union tail_pos
			(* If no, do nothing and recursively search the rest of the list *)
			else extract_symbols current_symbols tail_pos
	in

	(* In extract reachable function, we are keeping only the reachable rules in the grammar *)
	(* Use equal_sets function to compare the start symbol and embedded symbols inside *)
	let get_symbols = extract_symbols current_symbols filter_rules in 
	if (equal_sets (get_symbols) (extract_symbols (get_symbols) filter_rules)) 
	(* Find the reachable symbols *)
	then (get_symbols)
	(* Recursively search through the rest to find the reachable rules *)
	else extract_reachable (extract_symbols (get_symbols) filter_rules) filter_rules;;

(* If we are provided with input of a grammar which is formed by a start symbol and grammar rules *)
(* As for the output, we would like to return a expression which is formed by the start symbol followed 
by grammar rules with non-terminal symbols being removed *)
let rec filter_reachable g =
	(* Helper function 7 : Create a function that remove all the unreachable rules which are all the unreachable terminal elements *)
	(* First, we collect all the non-terminal symbols that are reachable *)
	(* Second, we are gonna remove all the non-terminal symbols that are unreachable *)
	let rec remove_all_unreachable output_rules the_head =
	let head_list = [the_head] in
	(* We are filtering the rules *)
	List.filter (fun elem -> check_list (extract_reachable head_list output_rules) (extract_left_tuple elem)) output_rules
	in
	(* We are returning the start symbol and the processed filtered grammar rules *)
	let right_side = extract_right_tuple g in
	let left_side = extract_left_tuple g in
	((left_side), remove_all_unreachable (right_side) (left_side));;



