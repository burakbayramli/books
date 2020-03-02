function [A, b, Eqin] = eliminateImpliedBoundsonRows(A, ...
    b, Eqin)
% Filename: eliminateImpliedBoundsonRows.m
% Description: the function is an implementation of the 
% presolve technique that eliminates implied bounds on 
% rows
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, b, Eqin] = eliminateImpliedBoundsonRows(A, ...
%   b, Eqin)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- b: vector of the right-hand side of the constraints 
%    (size m x 1)
% -- Eqin: vector of the type of the constraints 
%    (size m x 1)
%
% Output:
% -- A: presolved matrix of coefficients of the constraints 
%    (size m x n)
% -- b: presolved vector of the right-hand side of the 
%    constraints (size m x 1)
% -- Eqin: presolved vector of the type of the constraints 
%    (size m x 1)
 
[row, ~] = find(Eqin ~= 0); % find the inequality constraints
% find the number of these constraints
cardrows = length(row);
counterrows = 0;
for i = cardrows:-1:1 % for each of these constraints
	ii = row(i); % get the index of the constraint
	% if all elements in that constraint are less than or 
	% equal to zero, the constraint has a right-hand side 
	% greater than or equal to zero, and the constraint 
	% type is <=, then eliminate the constraint and update 
	% matrix A and vectors b and Eqin (Case 1)
	if all((A(ii, :) <= 0)) && (b(ii) >= 0)
        if Eqin(ii) == -1
			A(ii, :) = [];
			Eqin(ii) = [];
			b(ii) = [];
			counterrows = counterrows + 1;
        end
	% if all elements in that constraint are greater than 
	% or equal to zero, the constraint has a right-hand 
	% side less than or equal to zero, and the constraint 
	% type is >=, then eliminate the constraint and update 
	% matrix A and vectors b and Eqin (Case 1)
    elseif all((A(ii, :) >= 0)) && (b(ii) <= 0)
		if Eqin(ii) == 1
			A(ii, :) = [];
			Eqin(ii) = [];
			b(ii) = [];
			counterrows = counterrows + 1;
		end
	end
end
if counterrows > 0
	fprintf(['ELIMINATE IMPLIED BOUNDS ON ROWS: %i ' ... 
        'constraints were eliminated\n'], counterrows);
end
end