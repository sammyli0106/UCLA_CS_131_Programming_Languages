(* Test Cases for Task 1 : subset a b*)

(* Check for empty set case *)
let subset_test0 = subset [] [1;2;3;4;5]
(* Check for subset case 1 *)
let subset_test1 = subset [1;2;3;4] [1;2;3;4;5;6]
(* Check for subset case 2 *)
let subset_test2 = subset [4;8;5;9] [1;2;3;4;5;8;9]
(* Check for a not subset case *)
let subset_test3 = not (subset [1;2] [4;5;6;7])

(* Test Cases for Task 2 : equal_sets a b *)
(* Check for equal set case 1 *)
let equal_sets_test0 = equal_sets [1;2;3;4] [4;2;1;3]
(* Check for equal set case 2 *)
let equal_sets_test1 = equal_sets [] []
(* Check for not equal set case 1 *)
let equal_sets_test2 = not (equal_sets [1;2] [8;9;4])
(* Check for not equal set case 1 *)
let equal_sets_test3 = not (equal_sets [] [8;9;4])

(* Test Cases for Task 3 : set_union a b *)
(* Check for normal union case *)
let set_union_test0 = equal_sets (set_union [1;2;3] []) [1;2;3]
(* Check for equal union case *)
let set_union_test1 = equal_sets (set_union [1;2;3] [4;5;6]) [1;2;3;4;5;6]
(* Check for empty union case *)
let set_union_test2 = equal_sets (set_union [9] []) [9]

(* Test Cases for Task 4 : set_intersection a b *)
(* Check for empty intersection case *)
let set_intersection_test0 =
  equal_sets (set_intersection [] [4;5;6;7]) []
(* Check for normal intersection case *)
let set_intersection_test1 =
  equal_sets (set_intersection [6;5;4;3;2;1] [1;2;3]) [1;2;3]
(* Check for equal intersection case *)
let set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4;5] [5;4;3;2;1]) [1;2;3;4;5]

(* Test Cases for Task 5 : set_diff a b *)
(* Check for subset diff case *)
let set_diff_test0 = equal_sets (set_diff [3;2;1] [1;2;3;4]) []
(* Check for normal diff case 1 *)
let set_diff_test1 = equal_sets (set_diff [5;2;1;1;2] [1;2]) [5]
(* Check for equal diff case *)
let set_diff_test2 = equal_sets (set_diff [1;2;3;4] [1;2;3;4]) []
(* Check for normal diff case 2 *)
let set_diff_test3 = equal_sets (set_diff [6;7;8] []) [8;7;6]
(* Check for empty diff case *)
let set_diff_test4 = equal_sets (set_diff [] [3;4;5;6]) []

(* Test Cases for Task 6 *)
(* Check for fixed point 0 convergence *)
let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 5) 3000000000 = 0
(* Check for fixed point infinity convergence *)
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 9.) 8. = infinity
(* Check for fixed point  *)
let computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 25. = 1.

(* Test Cases for Task 7 *)
(* Based from the TA slides grammar example *)
(* Non-terminal : Subject, Name *)
(* Terminal : Plant, Animal, Fruit, Car *)
type object_nonterminals = 
	| Object | MovingObject | Plant | Animal | Fruit | Car

let object_grammar =
  Object,
  [Plant, [T"rose"];
   Animal, [T"bird"];
   Fruit, [T"apple"];
   Car, [T"BMW"];
   MovingObject, [N Animal];
   MovingObject, [N Fruit];
   MovingObject, [N Car];
(*    MovingObject, [N Plant]; *)
   Object, [N Plant];
   Object, [N MovingObject; T","; N Object]]

let object_test0 =
  filter_reachable object_grammar = object_grammar

let object_test1 =
  filter_reachable (MovingObject, List.tl (snd object_grammar)) =
    (MovingObject,
     [Animal, [T"bird"]; Fruit, [T"apple"]; Car, [T"BMW"];
      MovingObject, [N Animal]; MovingObject, [N Fruit]; MovingObject, [N Car]])

let object_test2 =
  filter_reachable (Animal, snd object_grammar) = (Animal, [Animal, [T"bird"]])











