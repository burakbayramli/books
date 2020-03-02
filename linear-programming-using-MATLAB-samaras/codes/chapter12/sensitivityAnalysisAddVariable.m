function [optimal] = ...
    sensitivityAnalysisAddVariable(A, c, b, ...
    BasicList, Anplus1, cnplus1)
% Filename: sensitivityAnalysisAddVariable.m
% Description: the function is an implementation of the 
% sensitivity analysis when adding a new variable
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [optimal] = ...
%   sensitivityAnalysisAddVariable(A, c, b, ...
%   BasicList, Anplus1, cnplus1)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- c: vector of coefficients of the objective function 
%    (size n x 1)
% -- b: vector of the right-hand side of the constraints 
%    (size m x 1)
% -- BasicList: vector of the indices of the basic 
%    variables (size 1 x m)
% -- Anplus1: the new column in the matrix of coefficients 
%    of the constraints (size m x 1)
% -- cnplus1: the cost coefficient of the new variable
%
% Output:
% -- optimal: a flag variable showing if the LP problem
%    remains optimal or not (0 - the current basis is 
%    not optimal, 1 - the current basis remains
%    optimal)

[~, n] = size(A); % the size of matrix A
% calculate the basis inverse
BasisInv = inv(A(:, BasicList));
% compute vector x
x = zeros(1, n);
x(BasicList) = BasisInv * b;
% calculate vectors w and s
w = c(BasicList)' * BasisInv;
s = c' - w * A;
% check if the current basis is not optimal
if any(x(BasicList) < 0)
	disp('The current basis is not optimal!')
	optimal = 0;
	return
end
if any(s < 0)
	disp('The current basis is not optimal!')
	optimal = 0;
	return
end
% find the value of the new reduced cost
Snplus1 = cnplus1 - w * Anplus1;
% check if the current basis remains optimal
if Snplus1 >= 0  
	disp('The current basis remains optimal!')
	optimal = 1;
	return
else
	disp('The current basis is not optimal!')
	optimal = 0;
	return
end
end