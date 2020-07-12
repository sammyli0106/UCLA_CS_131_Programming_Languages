plain_tower(5,
         [[2,3,4,5,1],
          [5,4,1,3,2],
          [4,1,5,2,3],
          [1,2,3,4,5],
          [3,5,2,1,4]],
         C).

plain_tower(5, T,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])).

plain_tower(5,
         [[2,3,4,5,1],
          [5,4,1,3,2],
          Row3,
          [RC41,5|Row4Tail],
          Row5],
         counts(Top, [4|BottomTail],
                [Left1,Left2,Left3,Left4,5],
                Right)).


% check_length predicate
check_length([], _).
check_length([head | _], N) :-
	length(head, N).

% within_domain predicate
within_domain(_, []).
within_domain(N, [head | tail]) :-
	fd_domain(head, 1, N),
	within_domain(N, tail). 

check_count(1, [T], height) :-
	T #> height.

check_count(0, [T], height) :-
	T #< height.

% Omit the case when count is 1 and 0
check_count(count, [head | tail], maxheight) :-
	head #> maxheight,
	updateCount #= count - 1,
	check_count(updateCount, tail, head).

check_count(count, [head | tail], maxheight) :-
	head #< maxheight,
	check_count(count, tail, maxheight).

verifycount([count | body], [list | tail]) :-
	check_count(count, list, 0),
	verifycount(body, tail).

verifycountrev([count | body], [list | tail]) :-
	reverse(list, revlist),
	check_count(count, revlist, 0),
	verifycount(body, tail).

% left T 
check_lr([], [], []).
check_lr(left, T, right) :-
	verifycount(left, T),
	% maplist(reverse, T, revT),
	verifycountrev(right, T).

check_tb([], [], []).
check_tb(top, tranT, bottom) :-
	verifycount(top, tranT),
	% maplist(reverse, tranT, tranTrev),
	verifycountrev(bottom, tranT).

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