% CSC324 Lab: Binary Search Trees in Prolog
% btree.pl

% Task 1: Count Nodes in BST
% count_nodes(Tree, N)
% N is the total number of nodes in the BST Tree

count_nodes(nil, 0).
count_nodes(t(Left, _, Right), N) :-
    count_nodes(Left, NLeft),
    count_nodes(Right, NRight),
    N is NLeft + NRight + 1.


% Task 2: Sum Keys in BST
% sum_keys(Tree, N)
% N is the total sum of all keys in the BST Tree

sum_keys(nil, 0).
sum_keys(t(Left, Key, Right), N) :-
    sum_keys(Left, SumLeft),
    sum_keys(Right, SumRight),
    N is SumLeft + SumRight + Key.


% Task 3: Height of a BST
% height(Tree, H)
% H is the total number of nodes along the longest path in the BST Tree

height(nil, 0).
height(t(Left, _, Right), H) :-
    height(Left, HLeft),
    height(Right, HRight),
    max_height(HLeft, HRight, MaxH),
    H is MaxH + 1.

% Helper predicate to find maximum of two heights
max_height(H1, H2, H1) :- H1 >= H2.
max_height(H1, H2, H2) :- H1 < H2.


% Task 4: Search a BST
% contains(Tree, K)
% K is the key being searched for in the BST Tree
% O(log n) time complexity

contains(t(_, Key, _), Key).
contains(t(Left, Key, _), K) :-
    K < Key,
    contains(Left, K).
contains(t(_, Key, Right), K) :-
    K > Key,
    contains(Right, K).


% Task 5: Does Not Contain
% not_contains(Tree, K)
% K is the key being searched for non-existence in the BST Tree
% O(log n) time complexity using negation as failure

not_contains(Tree, K) :-
    \+ contains(Tree, K).