function [optimal] = sensitivityAnalysisChangeb(A, ...
    c, b, BasicList, index, DeltaB)
% Filename: sensitivityAnalysisChangeb.m
% Description: the function is an implementation of the 
% sensitivity analysis when a change in the right-hand
% side occurs
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [optimal] = sensitivityAnalysisChangeb(A, ...
%   c, b, BasicList, index, DeltaB)
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
% -- index: the index of the constraint where the change
%    in the right-hand side occurs
% -- DeltaB: the amount of change in the right-hand side
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
% find the ranges of the right-hand side coefficients
[bl, bu] = sensitivityAnalysisb(A, b, BasicList);
% change the right-hand side of the specific constraint
b(index) = b(index) + DeltaB;
% check if the current basis remains optimal
if b(index) >= bl(index) && b(index) <= bu(index)
	disp('The current basis remains optimal!')
	optimal = 1;
	return
else
	disp('The current basis is not optimal!')
	optimal = 0;
	return
end
end