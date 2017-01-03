echo on
clc

% A permutation can be given two ways - as a series of numbers p or a matrix P.
% The first way, the numbers 1, 2, ..., n come in a (probably rearranged) order.
% The second way, the matrix I comes with its rows rearranged in the same order.
%  
% The short form is the permutation, say p = [2 3 1 4]. MATLAB makes it easy to
% produce the matrix form, just by P = I(p, :). Here I = eye(4). The rows are 
% reordered by p, the columns are not touched.
%  
% Suppose you want the transpose matrix P'. You will get it from I(:,p)!
% The transpose is also the inverse, for permutation matrices and any other
% orthogonal matrix Q. So we could pull out P inverse from the rows of P'. 
% Easier to proceed as follows with the row of numbers p; the inverse is q:

p = [2 3 1 4]
q(p) = 1:4
% For powers of p use p(p) and p(p(p)) or take matrix powers P*P and P*P*P.
% To multiply permutations, use matrices P2*P1 or work with  rows of numbers p:

p1 = p; p2 = [1 2 4 3]
product = p2(p)
% The SIGN of the permutation p is the same as the DETERMINANT of the matrix P.
% Therefore the codes signperm and permdet  produce the same answer. The input
% to both programs is p (not P). The codes create P = I(p,:) when they want to
% take the determinant of a matrix.  See permdet for exercises.

echo off


