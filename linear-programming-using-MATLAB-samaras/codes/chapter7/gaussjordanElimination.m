function [BasisInv] = gaussjordanElimination(A, BasicList)
% Filename: gaussjordanElimination.m
% Description: the function is an implementation of the 
% Gauss-Jordan elimination basis inverse method
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [BasisInv] = gaussjordanElimination(A, BasicList)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n) 
% -- BasicList: vector of indices of the basic variables
%   (size 1 x m)
%
% Output:
% -- BasisInv: matrix with the new basis inverse (size m x m)

BasisInv = A(:, BasicList) \ speye(length(BasicList));
end