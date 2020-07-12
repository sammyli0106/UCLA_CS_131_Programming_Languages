% helper function 1, check length for all four sides 
check_side_length(top, bottom, left, right, N) :-
	length(top, N),
	length(bottom, N),
	length(left, N),
	length(right, N). 

% helper function 2 & 3, limit the domain space
within_domain_1(N, T) :-
	maplist(domain_limit(N), T),
	% elements in the list have distinct values 
	maplist(fd_all_different, T).

% helper function, based from TA hint code
% domain_limit(N, []).
domain_limit(_, []).
domain_limit(N, [HD | TL]) :-
	fd_domain(HD, 1, N),
	domain_limit(N, TL).
	
within_domain_2(X) :-
	maplist(fd_all_different, X).

label_helper(T) :-
	maplist(fd_labeling, T).

% greater than case
greater_than(HD, maxHeight, TL, output) :-
	% HD #> maxHeight,
	HD #< maxHeight,
	temp #= output - 1,
	updatedOutput #= output - 1,
	find_count(HD, TL, updatedOutput).
	
% less than case 
less_than(HD, maxHeight, TL, output) :-
	% HD #< maxHeight,
	HD #> maxHeight,
	% body, count, tallest 
	find_count(maxHeight, TL, output).

% input, 0, head, output
find_count(_, [], 0).
find_count(maxHeight, [HD|TL], output) :-

	% either case, be careful about the ;
	% greater than case 
	greater_than(HD, maxHeight, TL, output);
	less_than(HD, maxHeight, TL, output).

% Refer to the TA slides 
% check_count([], N, _).
check_count([], _, _).
check_count([HD | TL], N, [head | rest]) :-
	find_count(0, HD, output),
	head is output,
	check_count(TL, N, rest).

check_count_lr(T, N, left, right) :-
	% check the count the match on each edge 
	% left column first 
	% T N list
	check_count(T, N, left),

	% reverse the input first 
	maplist(reverse, T, rever_T),

	% right column second, in reverse direction
	check_count(rever_T, N, right).

check_count_tb(X, N, top, bottom) :-
	% check the count the match on each edge 
	% top column first 
	check_count(X, N, top),

	% reverse the input first 
	maplist(reverse, X, rever_X),

	% bottom column second, in reverse direction
	check_count(rever_X, N, bottom).


% predicate tower that accept three arguments
% N refers to the size of the square grid
% T refers to list of N lists, each list refers to a row of the square grid 
% C refers to counts and 4 arguments, top, bottom, left, right edges

tower(N, T, C) :-
	% set up the counts from four sides
	C = counts(top, bottom, left, right),

	% check the length
	length(T, N),

	% check length function for the rest of the parts
	check_side_length(top, bottom, left, right, N),

	% Based from TA hint code for the usage of maplist
	% maplist allow goal apply to all element of the list
	% Take out the flipllength function
	% finish domain limits 
	within_domain_1(N, T),
	transpose(T, X),
	within_domain_2(X),
	check_count_lr(T, N, left, right),
	check_count_tb(X, N, top, bottom),
	label_helper(T).












% check_length predicate
check_length([], _).
check_length([head | _], N) :-
	length(head, N).

% within_domain predicate
within_domain(_, []).
within_domain(N, [head | tail]) :-
	fd_domain(head, 1, N),
	within_domain(N, tail). 

% Omit the case when count is 1 and 0
check_count(count, [head | tail], maxheight) :-
	head #< maxheight,
	updateCount #= count - 1,
	check_count(updateCount, tail, head);

	head #> maxheight,
	check_count(count, tail, maxheight).

verifycount([count | body], [list | tail]) :-
	check_count(count, list, 0),
	verifycount(body, tail).

% left T 
check_lr([], [], []).
check_lr(left, T, right) :-
	verifycount(left, T),
	maplist(reverse, T, revT),
	verifycount(right, revT).

check_tb([], [], []).
check_tb(tranT, top, bottom) :-
	verifycount(top, tranT),
	maplist(reverse, tranT, revT),
	verifycount(bottom, revT).

% predicate tower that accept three arguments
% N refers to the size of the square grid
% T refers to list of N lists, each list refers to a row of the square grid 
% C refers to counts and 4 arguments, top, bottom, left, right edges

tower(N, T, C) :-
	% check the length
	length(T, N),

	% set up the counts from four sides
	C = counts(top, bottom, left, right),

	check_length(T, N),
	maplist(fd_all_different, T),
	within_domain(N, T),

	transpose(T, tranT),

	check_length(tranT, N),
	maplist(fd_all_different, tranT),
	within_domain(N, tranT),

	% check left, right
	check_lr(left, T, right),

	% check top, bottom
	check_tb(top, tranT, bottom),

	% label predicate 
	maplist(fd_labeling, T).


% transpose function from TA hint code
% This is SWI-prolog old implementation
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).







