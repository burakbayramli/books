function [A, b, Eqin, infeasible] = ... 
    eliminateZeroRows(A, b, Eqin)
% Filename: eliminateZeroRows.m
% Description: the function is an implementation of the 
% presolve method that eliminates zero rows
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, b, Eqin, infeasible] = ...
%   eliminateZeroRows(A, b, Eqin)
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
% -- infeasible: flag variable showing if the LP is 
%    infeasible or not
 
infeasible = 0;
% compute the number of nonzero elements in each row
delrows = sum(spones(A')); 
% if any row has all its element equal to zero
if any(delrows == 0)
	% get the indices of all rows that have all their 
	% elements equal to zero
	id = int16(find(delrows == 0));
	% check if the LP problem is infeasible (Case 2)
	k = int16(find(Eqin(id) == -1));
	if any(b(id(k)) < 0) && ~isempty(k)
		infeasible = 1;
		return;
	else
		% check if the LP problem is infeasible (Case 4)
		k = int16(find(Eqin(id) == 1));
        if any(b(id(k)) > 0) && ~isempty(k)
			infeasible = 1;
			return;
		else
			% check if the LP problem is infeasible (Case 6)
			k = int16(find(Eqin(id) == 0)); 
            if any(b(id(k)) ~= 0) && ~isempty(k)
				infeasible = 1;
				return;
            end
        end
	end
	% find the indices of the nonzero rows
	idnnz = int16(find(delrows > 0));
	% update matrix A and vectors b and Eqin by deleting 
	% zero rows
	A = A(idnnz, :);
	b = b(idnnz);
	Eqin = Eqin(idnnz);
	fprintf(['ELIMINATE ZERO ROWS: %i zero-rows were ' ... 
        'eliminated\n'], length(id))
end
end