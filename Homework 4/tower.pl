% There are two basic cases
outputCount(_, 0, []).
outputCount(MaxHeight, Count, [Head|Tail]) :-
	% Greater than case, see another tower 
	MaxHeight #> Head,
	outputCount(MaxHeight, Count, Tail);
	% Less than case, we do not see another tower 
	MaxHeight #< Head, 
	UpdateCount #= Count - 1,
	outputCount(Head, UpdateCount, Tail).

% check count for one of the tower according to input 
verifycount([], []).
verifycount([SingleList| Tail], [Number|Body]) :-
	outputCount(0, Number, SingleList),
	verifycount(Tail, Body).

% check count predicate for left and right 
checkLeftRight(LEFT, RIGHT, T) :-
	verifycount(T, LEFT),

	% reverse the list 
	maplist(reverse, T, RevT),

	verifycount(RevT, RIGHT).

% check count predicate for top and bottom 
checkTopBottom(TOP, BOTTOM, RevT) :-
	verifycount(RevT, TOP),

	% reverse the list 
	maplist(reverse, RevT, Tnew),

	verifycount(Tnew, BOTTOM).

% check for length predicate
length_func([], _).
length_func([Head|Tail], N) :- 
	length(Head, N),
	length_func(Tail, N).

% make sure elements are distinct predicate
unique_func([], _).
unique_func([Head|Tail], N) :- 
	fd_all_different(Head),
	unique_func(Tail, N).

% define domain predicate 
within_domain([], _).
within_domain([Head | Tail], N) :-
	fd_domain(Head, 1, N),
	within_domain(Tail, N). 

% HW predicate 1 
% N refers to the size of the square grid
% T refers to list of N lists, each list refers to a row of the square grid 
% C refers to counts and 4 arguments, top, bottom, left, right edges

tower(N, T, C) :- 
	% check whether the number of N lists match with the size of the grid 
	length(T, N),
	% Set up the constraints
	C = counts(TOP, BOTTOM, LEFT, RIGHT),
	% check the length for the 4 directions 
	length_func(T, N),
	% set up the finite domain solver 
	within_domain(T, N),
	% make sure distinct elements in the grid 
	unique_func(T, N),

	% TA hint code source 
	% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
	% We have check the rows, then we need to transpose the column to check again 
	transpose(T, RevT),

	% Repeat from the above steps 
	length_func(RevT, N),
	within_domain(RevT, N),
	unique_func(RevT, N),

	% check count for left, right, top and bottom 
	checkLeftRight(LEFT, RIGHT, T),
	checkTopBottom(TOP, BOTTOM, RevT),

	% Labeling option for backtracking 
	maplist(fd_labeling, T).

% -----------------------------------------------------------

% check plain length predicate 
check_plain_length(TOP, BOTTOM, LEFT, RIGHT, N) :-
	length(TOP, N),
	length(BOTTOM, N),
	length(LEFT, N),
	length(RIGHT, N).

% Create plain matrix row predicate 
create_matrix_row([], _).
create_matrix_row([Head | Tail], N) :-
	Head is N,
	UpdateN is N - 1,
	create_matrix_row(Tail, UpdateN).

% Find count predicate, two cases 
% Same as tower predicate 
matrix_count([], _, 0).
matrix_count([Head | Tail], MaxHeight, Count) :-
	MaxHeight > Head, 
	matrix_count(Tail, MaxHeight, Count);
	MaxHeight < Head,
	UpdateCount is Count - 1,
	matrix_count(Tail, Head, UpdateCount).


% Use this predicate to initalize the input if it is not 
% Then pass it to the check count predicate 
memberAndCount(Head, List, T) :-
	member(Head, List),
	matrix_count(T, 0, Head).

% Use this predicate to set up before find out about the count 
set_up(List, N) :- 
	% check the length of the list 
	length(List, N),
	% create the rows from 1 to N 
	% A cut is needed because there is not point of back tracing here 
	% We only need to construct once 
	create_matrix_row(List, N), !.

% Set up matrix regards to length and count 
matrix(_, _, [], _).
matrix([HeadLeft | TailLeft], [HeadRight | TailRight], [HeadT | TailT], N) :-

	% Set up predicate 
	set_up(List, N), 
	
	% make sure the elements in the list are unique in this way  
	permutation(List, HeadT),

	% check the count for left and right 
	memberAndCount(HeadLeft, List, HeadT),

	% reverse the list 
	reverse(HeadT, PermuteListRev),

	% check the count for top and bottom 
	memberAndCount(HeadRight, List, PermuteListRev),

	% Recursively calling the matrix function for the rest of the grid 
	matrix(TailLeft, TailRight, TailT, N).

% predicate that check for length 
length_check(T, N) :-
	length(T, N).

% HW predicate 2 
plain_tower(N, T, C) :-

	% check whether the number of N lists match with the size of the grid 
	length_check(T, N),

	% Set up the constraints
	C = counts(TOP, BOTTOM, LEFT, RIGHT),

	% check the length for four direction 
	check_plain_length(TOP, BOTTOM, LEFT, RIGHT, N),
	
	% Set up the plain matrix for left and right 
	matrix(LEFT, RIGHT, T, N),

	% tranpose matrix to handle top and bottom 
	transpose(T, RevT),

	% Set up the plain matrix for top and bottom 
	matrix(TOP, BOTTOM, RevT, N).


% -----------------------------------------------------------

% HW predicate 3, ambiguous predicate 

ambiguous(N, C, T1, T2) :- 
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.

% -----------------------------------------------------------

% HW predicate 4, measure performance 

% Measure time for tower predicate based on cpu time 
time_tower(Time) :- 
	statistics(cpu_time, [StartTime | _]),
	tower(5, _, counts([3,2,3,5,1], [2,3,2,1,2], [3,2,2,1,2], [1,3,3,3,2])),
	statistics(cpu_time, [StopTime | _]),
	Time is StopTime - StartTime.

% Meausre time for plain tower predicate based on cpu time 
time_plain_tower(Time) :-
	statistics(cpu_time, [StartTime | _]),
	plain_tower(5, _, counts([3,2,3,5,1], [2,3,2,1,2], [3,2,2,1,2], [1,3,3,3,2])),
	statistics(cpu_time, [StopTime | _]),
	Time is StopTime - StartTime.

% Package up the test case predicate 
speedup(Ratio) :-
	time_plain_tower(PlainTime),
	time_tower(Time),
	Ratio is PlainTime / Time.

% -----------------------------------------------------------

% transpose function from TA hint code
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

