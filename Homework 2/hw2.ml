(* First, declare the necessarcy data structure for the assignment. *)
(* Type declaration for symbol *)
type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal 
	| T of 'terminal

(* Type declaration for parse_tree *)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


(* Helper function that return empty *)
let return_empty = [];;

(* Problem 1 : Write a function convert_grammar gram1 that return the Homework 2-style grammar and
it is converted from Homework 1 style grammar, test the implementation on test grammar from Homework 1 *)
(* First, I need to extract the wrapped value inside by using the following two functions : 
val fst : 'a * 'b -> 'a ; val snd : 'a * 'b -> 'b *)
(* Given a nonterminal symbol, we need to extract the alternative list for that symbol *)
let rec extract_alternative_list grammar_rules symbol = 
	match grammar_rules with 
	head_pos :: tail_pos ->
		(* Extract the first value from the head element *)
		let head_value_1 = fst head_pos in 
		(* Extract the second value from the head element *)
		let head_value_2 = snd head_pos in 
		let check_rest = extract_alternative_list tail_pos symbol in 
		(* If the head element is equal to the nonterminal term *)
		if (head_value_1 = symbol) then
			(* Append the second value to the rest of the list *)
			head_value_2 :: check_rest
		else
			(* Continue to recursively search *)
			check_rest
	(* We need to handle empty case which is when grammar is empty, we will return empty *)
	| [] -> return_empty;; 

let convert_grammar gram1 = 
	let first_elem = fst gram1 in 
	let second_elem = snd gram1 in
	(* Accept Homework 1 style and return Homework 2 style *)
	(first_elem, extract_alternative_list second_elem);;

(* Problem 2 : Write a function parse_tree_leaves tree that traverses the parser tree from left to right
and this should output a list of encountered leaves *)
let rec pre_parse_tree_leaves = function 
    (* Split the list into head element and tail list *)
	head_pos :: tail_pos -> 
		(* Handle the head element *)
		let front = handle_head head_pos in 
		(* Recursively call the function to handle the tail part *)
		let back = pre_parse_tree_leaves tail_pos in 
		front @ back 
	(* If the input list is empty, then just return empty *)
	| [] -> return_empty

and handle_head head = 
	match head with 
	(* Node Case, I need to handle the node by recursively searching through the subtree and find the leaves 
	by passing the terminal part where the leaves located at *)
	| Node (_, terminal_node) ->
		let move_on = pre_parse_tree_leaves terminal_node in 
		move_on
	(* Leaf Case, I need to print out the value by adding to the output list *)
	| Leaf terminal_leaf -> [terminal_leaf];;

(* I need to convert the tree input into a tree list *)
let parse_tree_leaves tree = pre_parse_tree_leaves [tree];;

(* Problem 3 : Write a make_matcher gram that returns a matcher for the grammar gram. 
When applied to an acceptor accept and a fragment frag, the matcher must try the grammar
rules in order and return result of calling accept on the suffix corresponding to the first acceptable
matching prefix of frag *)

(* Helper function that return None *)
let return_none = None;;

(* Helper function that return accept and frag *)
let return_accept_frag frag_input accept_input = accept_input frag_input;;

(* Modify more on this *)
let rec pre_matcher frag_input accept_input grammar = function 
	head_pos :: tail_pos -> 
	(* We are checking whether the first rule is a match among all the options in the 
	nonterminal list *)
	let check_head_element = search_level grammar head_pos accept_input frag_input in 
	(match check_head_element with 
		(* If head element is not a match then, we recursively checking whether the rest of the rule 
	 	is a match among all the options in the nonterminal list *)
		| None -> 
			let check_tail_element = pre_matcher frag_input accept_input grammar tail_pos in 
			check_tail_element

		(* If there is a matching with our head element which is prefix, our matcher just return whatever the 
		acceptor returned *)
		| return_input -> return_input)

	(* We need to handle the case when grammar rule is empty which is 
	we have already finished checking all the options in the nonterminal list *)
	| [] -> return_none

(* The second recursion function will check for the current rule *)
and search_level grammar grammar_rule accept_input frag_input = 
	match grammar_rule with 
	(* First, we check whether the list is empty and if it is empty, then we just return the input *)
	| [] -> return_accept_frag frag_input accept_input
	(* If the head element of current grammar rule is an nonterminal symbol, we try to match the
	prefix by recusrively searching for the nonterminal symbol *)
	| (N nonterminal_term) :: rest -> handle_nonterminal_term grammar accept_input frag_input nonterminal_term rest
	(* If the head element of current grammar rule is a terminal symbol, we need to check if the 
	symbol match with frag input *)
	| (T terminal_term) :: rest -> 
		match frag_input with 
		(* Attempting to match the frag elements with current grammar rule elements *)
		| head_pos :: tail_pos -> handle_terminal_term head_pos tail_pos terminal_term grammar rest accept_input
		(* If the grammar rule is empty, then just return none *)
		| [] -> return_none

(* This is the helper function that handle non terminal case *)
and handle_nonterminal_term gram_in_1 accept_in_1 frag_in non_term remain_1 =  
							let nonterminal_part = gram_in_1 non_term in
							let check_remain = search_level gram_in_1 remain_1 accept_in_1 in 

							(* When are checking the nonterminal, we need to track the value at the same time *)
							pre_matcher frag_in check_remain gram_in_1 nonterminal_part

(* This is the helper function that handle terminal case *)
and handle_terminal_term head_elem tail_elem term_term gram_in_2 remain_2 accept_in_2 =

							(* If the head element is equal to frag input, then we will continue to search recursively *) 
							if (head_elem = term_term) 
							then search_level gram_in_2 remain_2 accept_in_2 tail_elem
							(* If not, return none *)
							else return_none;;


(* Function that return a match for a given grammar *)
let make_matcher gram acceptor_input fragment_input =
	let tail_expression = snd gram in
	let head_expression = fst gram in 
	(pre_matcher fragment_input acceptor_input tail_expression (tail_expression head_expression));;


(* Problem 4 : Write a function that make_parser gram that returns a parser for the grammar gram. When applied to a 
fragment frag, the parser returns an optional parse tree. If frag cannt be parsed entirely, the parser returns None.
Otherwise, it returns Some tree where tree is the parse tree corresponding to the input fragment. Attempt the grammar rule
in the same order *)
(* Similar to the make matcher above and only one place changed *)

(* Helper function that return accept and frag *)
let return_accept_frag_parser accept_input frag_input = accept_input frag_input;;

let rec pre_make_parser frag_input accept_input grammar = function
		head_pos :: tail_pos -> 
		(* We are checking whether the first rule is a match among all the options in the 
		nonterminal list *)
		let match_head_element = search_parser_level grammar head_pos accept_input frag_input in 
			(match match_head_element with 
				(* Return Some tree where tree is the specific parse tree with the 
				input fragment *)
				(* This is the only one major modification line compare with the block of code from above *)
				| Some elem -> Some (head_pos :: elem)
				(* If head element is not a match then, we recursively checking whether the rest of the rule 
	 			is a match among all the options in the nonterminal list *)
				| None -> 
					let match_tail_element = pre_make_parser frag_input accept_input grammar tail_pos in 
					match_tail_element)
		(* We need to handle the case when grammar rule is empty which is 
		we have already finished checking all the options in the nonterminal list *)
		| [] -> return_none

and search_parser_level grammar grammar_rule accept_input frag_input = 
	match grammar_rule with 
	(* First, we check whether the list is empty and if it is empty, then we just return the input *)
	| [] -> return_accept_frag_parser accept_input frag_input
	(* If the head element of current grammar rule is an nonterminal symbol, we try to match the
	prefix by recusrively searching for the nonterminal symbol *)
	| (N nonterminal_term) :: rest -> handle_nonterminal_term_parser grammar accept_input frag_input nonterminal_term rest
	(* If the head element of current grammar rule is a terminal symbol, we need to check if the 
	symbol match with frag input *)
	| (T terminal_term) :: rest -> 
		match frag_input with 
		(* Attempting to match the frag elements with current grammar rule elements *)
		| head_pos :: tail_pos -> handle_terminal_term_parser head_pos tail_pos terminal_term grammar rest accept_input
		(* If the grammar rule is empty, then just return none *)
		| [] -> return_none

(* This is the helper function that handle non terminal case *)
and handle_nonterminal_term_parser gram_input acceptor_in fragment_in nonter_term left_over = 
							let non_terminal_section = gram_input nonter_term in
							let check_rest = search_parser_level gram_input left_over acceptor_in in
							(pre_make_parser fragment_in check_rest gram_input non_terminal_section)

(* This is the helper function that handle terminal case *)
and handle_terminal_term_parser head_element tail_element ter_term grammar_input rest accept_input = 
							let continue_search = search_parser_level grammar_input rest accept_input tail_element in 
							if (ter_term = head_element)
							then continue_search
							else return_none;;

(* This is a function that will create a parse tree based from an input fragment *)
(* Input : fragment, start symbol which is the root of the tree *)
let rec build_tree input_frag_list = function 
		(* If the root of the tree is empty, then costruct the node with 
		fragment and with empty terminal list *)
		[] -> (input_frag_list, return_empty)
		(* If the root of the tree is not empty, then separate the list into 
		head element and tail segment *)
		| head_pos :: tail_pos ->
			(* From here, we are buidling all possible paths to all the leaves within the parse tree 
			in order to create nodes for all possible leaves. *)
			let route = search_subtree input_frag_list head_pos in 
			match route with 
				(* First level route *)
				(given_frag_input_1, current_route) -> 
				let other_route = build_tree given_frag_input_1 tail_pos in 
				match other_route with 
					(* Second level route *)
					(given_frag_input_2, other_route) ->
						let entire_route = current_route :: other_route in 
						(given_frag_input_2, entire_route)

and search_subtree input_list start_node = 
	match input_list with 
		| head_pos :: tail_pos -> 
			(match start_node with 
				(* We are at nonterminal now, and try to construct all possible parse subtree *)
				| (N nonterminal_node) ->
					(* For nonterminal node, if the list of rules are not empty, then create a node based
					from current position *)
					(let node = build_tree tail_pos head_pos in 
					(match node with 
						| (match_1, match_2) -> 
							let current_node_1 = Node (nonterminal_node, match_2) in 
							(match_1, current_node_1)))
				(* We are at terminal now, and try to construct all possible parse subtree *)
				| (T terminal_node) -> 
					(* For terminal node, if the list of rules are not empty, create a leaf based from the terminal
					that we are locating *) 
					(let current_leaf = Leaf terminal_node in 
					let path = head_pos :: tail_pos in 
					(path, current_leaf)))
		| [] -> match start_node with 
				(* For nonterminal node, if list of rules are empty, then create a node with empty terminal part*)
				| (N nonterminal_node) -> 
					(let current_node_2 = Node (nonterminal_node, return_empty) in 
					(return_empty, current_node_2))
				(* For terminal node, if the list of rules are empty, then the nonterminal part is empty *)
				| (T terminal_node) -> (return_empty, Leaf terminal_node)

(* Prepare the input for the make_parser function *)
let rec pre_parser_input gram_input frag_input = 
	(iter_function gram_input accept_empty frag_input)
and iter_function grammar accept_input frag_input = 
	let get_first_val = snd grammar in 
	(pre_make_parser frag_input accept_input get_first_val (get_first_val (fst grammar)))
(* A helper function that accept empty input for the acceptor that will be used later 
in parser function *)
and accept_empty suffix = 
	match suffix with 
		[] -> Some return_empty 
		| _ -> return_none;;  

(* The final parser function *)
let make_parser gram = fun frag_input -> 
	let pattern = pre_parser_input gram frag_input in 
	match pattern with 
		(* If a given fragment's return pattern is none, then return None *)
		| None -> return_none
		(* If the given fragment input are not able to be parsed entirely, then we are
		gonna return none *)
		| Some [] -> return_none
		(* If the given fragment input could be parsed, we are gonna use it to construct the
		parse tree *)
		| Some in_list -> 
			(let extract_first_val = fst gram in 
			 let start_symbol = [N extract_first_val] in 
			 let input_tree = build_tree in_list start_symbol in 
				match input_tree with 
				| (_, suffix_term) -> 
					(* Check the suffix part *)
					match suffix_term with
						| head_pos :: tail_pos -> Some head_pos
						| [] -> return_none);;

