function [A, c] = eliminateRedundantColumns(A, c, b, Eqin)
% Filename: eliminateRedundantColumns.m
% Description: the function is an implementation of the 
% presolve technique that eliminates redundant columns
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c] = eliminateRedundantColumns(A, c, b, Eqin)
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
%
% Output:
% -- A: presolved matrix of coefficients of the constraints 
%    (size m x n)
% -- c: presolved vector of coefficients of the objective 
%    function (size n x 1)

[row, ~] = find(Eqin == 0); % find the equality constraints
% find the constraints with a zero right-hand side
[row1, ~] = find(b == 0);
% get the intersection of these sets, i.e., the equality 
% constraints with a zero right-hand side
idsrows = intersect(row, row1);
% find the number of these constraints
cardrows = length(idsrows);
countercols = 0;
if cardrows > 0 % if such constraints exist
	for i = cardrows:-1:1 % for each of these constraints
		ii = idsrows(i); % get the index of the constraint
        % find the nonzero elements of this row
		[row, j] = find(A(ii, :));
		if ~isempty(row) % if there are nonzero elements
			% if all elements are greater than zero, 
            % eliminate the columns and update matrix A 
            % and vector c
			if all(A(ii, j) > 0)
				c(j) = [];
				A(:, j) = [];
				countercols = countercols + length(j);
			% if all elements are less than zero, eliminate 
            % the columns and update matrix A and vector c
            elseif all(A(ii, j) < 0)
				c(j) = [];
				A(:, j) = [];
				countercols = countercols + length(j);
			end
		end
	end
	if countercols > 0
		fprintf(['ELIMINATE REDUNDANT COLUMNS: %i ' ...
            'redundant columns were eliminated' ...
            '\n'], countercols);
	end
end
end