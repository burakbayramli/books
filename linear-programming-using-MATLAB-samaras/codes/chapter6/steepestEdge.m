function [index] = steepestEdge(Sn, NonBasicList, ...
    A, BasisInv)
% Filename: steepestEdge.m
% Description: the function is an implementation of the 
% Steepest Edge pivoting rule
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [index] = steepestEdge(Sn, NonBasicList, ...
%   A, BasisInv)
% 
% Input:
% -- Sn: vector of reduced costs (size 1 x (n - m))
% -- NonBasicList: vector of indices of the nonbasic 
%    variables (size 1 x (n - m)) 
% -- A: matrix of coefficients of the constraints 
%    (size m x n) 
% -- BasisInv: matrix with the basis inverse (size m x m)
%
% Output:
% -- index: the index of the entering variable
 
% calculate the denominator of the equation
Y = BasisInv * A(:, NonBasicList);
nd = sqrt(1 + diag(Y' * Y));
% calculate the index of the minimum value of the division 
% of the vector Sn to vector nd
[~, j] = min(Sn'./ nd);
index = j(1);
end