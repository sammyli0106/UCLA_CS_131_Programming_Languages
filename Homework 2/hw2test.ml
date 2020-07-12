(* make_matcher testcase : non trivial, in the style of the given testcases, but cover
different problem areas with a gramma of my own. *)

(* make_parser testcase : using the same grammar, to check parse_tree_leaves is the inverse of
make_parser_gram which mean when make_parser gram frag return Some tree, then parse_tree_leaves tree
equals frag *)

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

(* Define my own terminals *)
type human_nonterminals =
  | Human | Person | Man | Woman | Teenager | Children 

let human_gram =
  (Human,
   function
     | Human ->
         [[N Person; N Teenager; N Human];
          [N Person]]
     | Person ->
	 [[N Children];
	  [N Man];
	  [N Woman; N Man];
	  [N Man; N Woman];
	  [T"("; N Human; T")"]]
     | Man ->
	 [[T"@"; N Human]]
     | Woman ->
	 [[T"&"]]
     | Teenager ->
	 [[T"*"];
	  [T"**"]]
     | Children ->
	 [[T"a"]; [T"b"]; [T"c"]; [T"d"]; [T"e"]])

let test0 =
  ((make_matcher human_gram accept_all ["bad output"]) = None)

let test1 =
  ((make_matcher human_gram accept_all ["c"])
   = Some [])

let test2 =
  ((make_matcher human_gram accept_all ["d"; "*"; "@"; "c"; "*"])
   = Some ["*"])

let test3 =
  ((make_matcher human_gram accept_empty_suffix ["b"; "*"; "@"; "e"; "*"])
   = None)

let test4 =
  (parse_tree_leaves (Node ("*", [Leaf "a"; Node ("-", [Leaf "b"; Leaf "c"])]))
   = ["a"; "b"; "c"])

let frag_in = ["@"; "b"; "&"; "**"; "e"]

let test5 =
  match make_parser human_gram frag_in with
    | Some tree -> parse_tree_leaves tree = frag_in
    | _ -> false