
/*
P01 (*) Find the last element of a list.
Example:
?- my_last(X,[a,b,c,d]).
X = d
*/
my_last(X, [X]).
my_last(X, [_|L]) :- my_last(X, L).

/*
P02 (*) Find the last but one element of a list.
(zweitletztes Element, l'avant-dernier élément)
*/
second_last(X,[X,_]).
second_last(X,[_,Y|Ys]) :- second_last(X,[Y|Ys]).

/*
P03 (*) Find the K'th element of a list.
The first element in the list is number 1.
Example:
?- element_at(X,[a,b,c,d,e],3).
X = c
*/
element_at(X, [X | _], 1).
element_at(X, [_ | Y], N) :- N > 1, K is N - 1, element_at(X, Y, K).

% P04 (*) Find the number of elements of a list.
num_elements([], 0).
num_elements([_ | Y], N) :- num_elements(Y, K), N is K + 1.


% P05 (*) Reverse a list.
nmy_reverse(L1,L2) :- my_rev(L1,L2,[]).

my_rev([],L2,L2) :- !.
my_rev([X|Xs],L2,Acc) :- my_rev(Xs,L2,[X|Acc]).


% P06 (*) Find out whether a list is a palindrome.
% A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
is_palendrome(L) :- reverse(L, L).

%% (**) Flatten a nested list structure.
%% Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

%% Example:
%% ?- my_flatten([a, [b, [c, d], e]], X).
%% X = [a, b, c, d, e]

my_flatten(X,[X]) :- \+ is_list(X).
my_flatten([],[]).
my_flatten([X|Xs],Zs) :- my_flatten(X,Y), my_flatten(Xs,Ys), append(Y,Ys,Zs).

%% (**) Eliminate consecutive duplicates of list elements.
%% If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

%% Example:
%% ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%% X = [a,b,c,a,d,e]

compress([], []).
compress([A], [A]).
compress([A, A | Cs], Zs) :- compress([A | Cs], Zs).
compress([A, B | Cs], [A | Zs]) :- A \= B, compress([B | Cs], Zs).

%% (**) Pack consecutive duplicates of list elements into sublists.
%% If a list contains repeated elements they should be placed in separate sublists.

%% Example:
%% ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%% X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]

% simple base cases
pack([], []).
pack([A], [A]).
% list with many of one element
%pack([A, B | Bs], [[A, A | As] | Cs]).


%% P14 (*) Duplicate the elements of a list.
%% Example:
%% ?- dupli([a,b,c,c,d],X).
%% X = [a,a,b,b,c,c,c,c,d,d]
dupli([], []).
dupli([X], [X, X]).
dupli([X | Xs], Ys) :- dupli([X], A), dupli(Xs, As), append(A, As, Ys).


% P15 (**): Duplicate the elements of a list agiven number of times

% dupli(L1,N,L2) :- L2 is obtained from L1 by duplicating all elements
%    N times.
%    (list,integer,list) (?,+,?)

dupli(L1,N,L2) :- dupli(L1,N,L2,N).

% dupli(L1,N,L2,K) :- L2 is obtained from L1 by duplicating its leading
%    element K times, all other elements N times.
%    (list,integer,list,integer) (?,+,?,+)

dupli([],_,[],_).
dupli([_|Xs],N,   Ys, 0) :- dupli(Xs,N,Ys,N).
dupli([X|Xs],N,[X|Ys],K) :- K > 0, K1 is K - 1, dupli([X|Xs],N,Ys,K1).
