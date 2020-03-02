function [optimal] = ...
    sensitivityAnalysisAddConstraint(A, c, b, ...
    BasicList, Amplus1, bmplus1, constrEqin)
% Filename: sensitivityAnalysisAddConstraint.m
% Description: the function is an implementation of the 
% sensitivity analysis when adding a new constraint
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [optimal] = ...
%   sensitivityAnalysisAddConstraint(A, c, b, ...
%   BasicList, Amplus1, bmplus1, constrEqin)
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
% -- Amplus1: the new row in the matrix of coefficients 
%    of the constraints (size 1 x n)
% -- bmplus1: the right-hand side value of the new 
%    constraint
% -- constrEqin: the type of the new constraint (-1: 
%    constraint type is 'less than or equal to', 0: 
%    equality constraint, 1: constraint type is 
%    'greater than or equal to')
%
% Output:
% -- optimal: a flag variable showing if the LP problem
%    remains optimal or not (0 - the current basis is 
%    not optimal, 1 - the current basis remains
%    optimal)

[m, n] = size(A); % the size of matrix A
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
% add the new constraint
b = [b; bmplus1];
c = [c; 0];
if constrEqin == -1 % 'less than or equal to' constraint
	A = [A zeros(m, 1); Amplus1 1];
elseif constrEqin == 0 % equality constraint
	% we do not need to add a new variable, but
	% we add a new variable for simplicity's sake
	A = [A zeros(m, 1); Amplus1 0];
% 'greater than or equal to' constraint
elseif constrEqin == 1
	A = [A zeros(m, 1); Amplus1 -1];
end
% add the new constraint to the basic list
BasicList = [BasicList n + 1];
% calculate the new basis inverse
BasisInv = inv(A(:, BasicList));
% compute the new vector x
x = zeros(1, n + 1);
x(BasicList) = BasisInv * b;
% check if the current basis is not optimal
if x(n + 1) >= 0
	disp('The current basis remains optimal!')
	optimal = 1;
	return
else
	disp('The current basis is not optimal!')
	optimal = 0;
end
end