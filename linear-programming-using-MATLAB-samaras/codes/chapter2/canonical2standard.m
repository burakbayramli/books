function [A, c, b, Eqin, MinMaxLP] = ...
    canonical2standard(A, c, b, Eqin, MinMaxLP)
% Filename: canonical2standard.m
% Description: the function is an implementation of the 
% transformation of an LP problem in its canonical form 
% to its standard form
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, Eqin, MinMaxLP] = ...
%   canonical2standard(A, c, b, Eqin, MinMaxLP)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- c: vector of coefficients of the objective function 
%    (size n x 1)
% -- b: vector of the right-hand side of the constraints 
%    (size m x 1)
% -- Eqin: vector of the type of the constraints 
%    (size m x 1)
% -- MinMaxLP: the type of optimization
%
% Output:
% -- A: transformed matrix of coefficients of the 
%    constraints (size m x n)
% -- c: transformed vector of coefficients of the objective 
%    function (size n x 1)
% -- b: transformed vector of the right-hand side of the 
%    constraints (size m x 1)
% -- Eqin: transformed vector of the type of the 
%    constraints (size m x 1)
% -- MinMaxLP: the type of optimization

[m, n] = size(A); % size of matrix A
% find all constraints that are not equalities
for i = 1:m
	% transform constraints in the form 'less than or
	% equal to' to equality constraints
	if Eqin(i) == -1
		A = [A zeros(m, 1)];
		A(i, n + 1) = 1; 
		c = [c; 0];
		Eqin(i) = 0;
		n = n + 1;
	end
end
end
