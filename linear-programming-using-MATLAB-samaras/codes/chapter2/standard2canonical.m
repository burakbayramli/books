function [A, c, b, Eqin, MinMaxLP] = ...
    standard2canonical(A, c, b, Eqin, MinMaxLP)
% Filename: standard2canonical.m
% Description: the function is an implementation of the 
% transformation of an LP problem in its standard form 
% to its canonical form
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, Eqin, MinMaxLP] = ...
%   standard2canonical(A, c, b, Eqin, MinMaxLP)
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

[m, ~] = size(A); % size of matrix A
% find all constraints that are not in the form 'less than
% or equal to'
for i = 1:m
	if Eqin(i) == 0 % transform equality constraints
		f = find(A(i, :) > 0);
		f = f(1);
        b(i) = b(i) / A(i, f);
		A(i, :) = A(i, :) / A(i, f);
		Eqin(i) = -1;
		A(:, f) = [];
		c(f) = [];
	end
end
end