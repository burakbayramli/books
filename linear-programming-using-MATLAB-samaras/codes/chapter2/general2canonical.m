function [A, c, b, Eqin, MinMaxLP, c0] = ...
    general2canonical(A, c, b, Eqin, MinMaxLP, c0)
% Filename: general2canonical.m
% Description: the function is an implementation of the 
% transformation of a general LP problem to its
% canonical form
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, Eqin, MinMaxLP, c0] = ...
%   general2canonical(A, c, b, Eqin, MinMaxLP, c0)
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
% -- c0: constant term of the objective function
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
% -- c0: updated constant term of the objective function

[m, ~] = size(A); % size of matrix A
% if the LP problem is a maximization problem, transform it 
% to a minimization problem
if MinMaxLP == 1
	MinMaxLP = -1;
	c = -c;
end
% find all constraints that are not in the form 'less than
% or equal to'
for i = 1:m
	% transform constraints in the form 'greater than or
	% equal to'
	if Eqin(i) == 1
		A(i, :) = -A(i, :);
		b(i) = -b(i);
		Eqin(i) = -1;
	elseif Eqin(i) == 0 % transform equality constraints
		f = find(A(i, :) ~= 0);
		f = f(1);
		b(i) = b(i) / A(i, f);
		A(i, :) = A(i, :) / A(i, f);
        b([1:i - 1, i + 1:m]) = b([1:i - 1, i + 1:m]) - ...
            A([1:i - 1, i + 1:m], f) .* b(i);
        A([1:i - 1, i + 1:m], :) = ...
            A([1:i - 1, i + 1:m], :) - ...
            A([1:i - 1, i + 1:m], f) * A(i, :);
        c0 = c0 + c(f) * b(i);
        c = c - c(f) * A(i, :);
        A(:, f) = [];
		c(f) = [];
        Eqin(i) = -1;
	end
end
end