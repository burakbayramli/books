function [BasisInv] = luInverse(A, BasicList)
% Filename: luInverse.m
% Description: the function is an implementation of the LU
% Decomposition method to find the inverse of a matrix
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [BasisInv] = luInverse(A, BasicList)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n) 
% -- BasicList: vector of indices of the basic variables
%   (size 1 x m)
%
% Output:
% -- BasisInv: matrix with the new basis inverse (size m x m)

% compute L and U matrices
[L, U] = lu(A(:, BasicList));
% calculate the basis inverse
BasisInv = U \ (L \ (speye(length(BasicList))));
end