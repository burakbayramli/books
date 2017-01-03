function Y_big = vec(Y)
% PURPOSE: Create a matrix stacking the columns of Y
% ------------------------------------------------------------
% SYNTAX: Y_big = vec(Y)
% ------------------------------------------------------------
% OUTPUT: Y_big, vectorized matrix (n*k-by-1 vector)
% ------------------------------------------------------------
% INPUT: Y, n-by-k matrix
% ------------------------------------------------------------
% Note: vec does not check to make sure that you have a two-dimenional
% array.  It will vectorize any array, but there are no guarantees as to
% how it will be stacked.  If the input, Y, is n-by-k, the program will
% stack the columns beginning with the first.

% written by:
% Kyle K. Hood
% Yale University

n = numel(Y) ;
Y_big = reshape(Y,[n,1]) ;