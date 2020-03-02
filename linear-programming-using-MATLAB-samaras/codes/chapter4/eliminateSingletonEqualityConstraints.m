function [A, c, b, Eqin, c0, infeasible] = ...
	eliminateSingletonEqualityConstraints(A, c, b, Eqin, c0)
% Filename: eliminateSingletonEqualityConstraints.m
% Description: the function is an implementation of the 
% presolve technique that eliminates singleton equality 
% constraints
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, Eqin, c0, infeasible] = ...
%	eliminateSingletonEqualityConstraints(A, c, b, Eqin, c0)
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
% -- c0: constant term of the objective function
%
% Output:
% -- A: presolved matrix of coefficients of the constraints 
%    (size m x n)
% -- c: presolved vector of coefficients of the objective 
%    function (size n x 1)
% -- b: presolved vector of the right-hand side of the 
%    constraints (size m x 1)
% -- Eqin: presolved vector of the type of the constraints 
%    (size m x 1)
% -- c0: updated constant term of the objective function
% -- infeasible: flag variable showing if the LP is 
%    infeasible or not
 
infeasible = 0;
% compute the number of nonzero elements in each row
delrows = sum(spones(A'));
% find the rows that have only one nonzero element and set 
% all the other rows equal to zero
singleton = (delrows == 1);
% find the number of the rows that have only one nonzero 
% element
cardrows = nnz(singleton);
if cardrows > 0 % if singleton rows exist
	% get the indices of the singleton rows
	idsrows = int16(find(singleton));
	% find equality constraints
	[row, ~] = find(Eqin == 0);
	% compute the indices of the singleton equality 
    % constraints
	idsrows = intersect(idsrows, row);
	% compute the number of the singleton equality 
    % constraints
	cardrows = length(idsrows);
    % for each singleton equality constraint
	for i = cardrows:-1:1
		ii = idsrows(i); % get the index of the constraint
        % find the nonzero elements of this row
		[row1, j] = find(A(ii, :));
        if ~isempty(row1) % if there are nonzero elements
			b(ii) = b(ii) / A(ii, j); % update b
            % if b is greater than or equal to zero
            if b(ii) >= 0
				b_temp = b(ii) .* A(:, j); 
				A(:, j) = []; % delete column from A
				if c(j) ~= 0 % update c and c0
					c(j) = c(j) * b(ii);
					c0 = c0 - c(j);
				end
				% update matrix A and vectors c, b, and Eqin
				c(j) = [];
				b = b - b_temp;
				A(ii, :) = [];
				b(ii) = [];
				Eqin(ii) = [];
			else % the LP problem is infeasible (Case 2)
				infeasible = 1;
				return;
            end
        end
	end
    fprintf(['ELIMINATE SINGLETON EQUALITY CONSTRAINTS: ' ... 
        '%i singleton equality constraints were ' ....
        'eliminated\n'], cardrows);
end
end