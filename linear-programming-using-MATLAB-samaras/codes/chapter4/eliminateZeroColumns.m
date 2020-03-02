function [A, c, unbounded] = eliminateZeroColumns(A, c)
% Filename: eliminateZeroColumns.m
% Description: the function is an implementation of the 
% presolve method that eliminates zero columns
% Authors: Ploskas, N., & Samaras, N.
% 
% Syntax: [A, c, unbounded] = eliminateZeroColumns(A, c)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- c: vector of coefficients of the objective function 
%    (size n x 1)
%
% Output:
% -- A: presolved matrix of coefficients of the constraints 
%    (size m x n)
% -- c: presolved vector of coefficients of the objective 
%    function (size n x 1)
% -- unbounded: flag variable showing if the LP is unbounded
%    or not
 
unbounded = 0;
% find the columns that have all their elements equal to zero
delcols = (max(abs(A)) == 0)';
% if any column has all its element equal to zero
if any(delcols == 1)
	% get the indices of all columns that have all their 
	% elements equal to zero
	idelcols = int16(find(delcols));
	% check if the LP problem is infeasible (Case 2)
	if any(c(idelcols) < 0)
		unbounded = 1;
		return;
	end
	% get the indices of all columns that have at least one 
	% nonzero element
	idnnz = int16(find(1 - delcols));
	% update matrix A and vector c by deleting zero columns
	A = A(:, idnnz);
	c = c(idnnz);
	fprintf(['ELIMINATE ZERO COLUMNS: %i zero columns ' ...
        'were eliminated\n'], nnz(delcols))
end
end