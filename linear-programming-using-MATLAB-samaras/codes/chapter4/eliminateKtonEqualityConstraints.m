function [A, c, b, Eqin, c0, infeasible] = ...
	eliminateKtonEqualityConstraints(A, c, b, Eqin, ...
    c0, kton)
% Filename: eliminateKtonEqualityConstraints.m
% Description: the function is an implementation of the 
% presolve technique that eliminates kton equality 
% constraints
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, Eqin, c0, infeasible] = ...
%	eliminateKtonEqualityConstraints(A, c, b, Eqin, ...
%   c0, kton)
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
% -- kton: the type of equality constraints to eliminate (1:
%    singleton, 2: doubleton, etc.)
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

if kton == 1
    [A, c, b, Eqin, c0, infeasible] = ...
        eliminateSingletonEqualityConstraints(A, c, b, Eqin, c0);
else
	infeasible = 0;
	[m, n] = size(A);
	tol = max(m, n) * eps * norm(A, 'inf'); % compute tolerance
	colindex = sparse(1, n);
	ci = 1;
	eliminatedRows = 0;
	for k = kton:-1:1 % for each k, 1 <= k <= kton
		% compute the number of nonzero elements in each row
		delrows = sum(spones(A'));
		% find the rows that have only k nonzero elements and 
		% set all the other rows equal to zero
		singleton = (delrows == k);
		% find the number of the rows that have only k 
		% nonzero elements
		cardrows = nnz(singleton);
		% if kton rows exist
		if cardrows >= max(1, .01 * size(A, 1))
			% get the indices of the kton rows
			idsrows = int16(find(singleton));
			% find equality constraints
			[row , ~] = find(Eqin == 0);
			% compute the indices of the kton equality 
			% constraints
			idsrows = intersect(idsrows, row);
			% compute the number of the kton equality 
			% constraints
			cardrows = length(idsrows);
			% for each kton equality constraint
			for i = cardrows:-1:1
				% get the index of the constraint
				ii = idsrows(i);
				% find the nonzero elements of this row
				[~, j] = find(A(ii, :));
				% find the column of the last nonzero element
				if length(j) > 1
					j = j(length(j));
				elseif isempty(j)
					continue;
				end
				% set to zero if the value is less than or
				% equal to tol and continue to the next
				% constraint
				if abs(A(ii, j)) <= tol
					A(ii, j) = 0;
					continue;
				else % update matrix A and vectors c, b, 
					 % and Eqin
					b(ii) = b(ii) / A(ii, j);
					A(ii, :) = A(ii, :) / A(ii, j);
					i_nz = find(A(:, j));
					i_nz = setdiff(i_nz, ii);
					if ~isempty(i_nz)
						for t = i_nz
							if b(ii) ~= 0
								b(t) = b(t) - A(t, j) * b(ii);
							end
							A(t, :) = A(t, :) - A(t, j) * A(ii, :);
							Eqin(ii) = -1;
							colindex(ci) = j;
							ci = ci + 1;
						end
					else
						Eqin(ii) = -1;
						colindex(ci) = j;
						ci = ci + 1;
					end
					if c(j) ~= 0
						c0 = c0 + c(j) * b(ii);
						c = c - c(j) * A(ii, :)';
					end
					% the LP problem is infeasible (Case 2)
					if (k == 1) && (b(ii) < 0)
						infeasible = 1;
						return;
					end
					if (k == 1) && (b(ii) >= 0)
						% delete the iith constraint
						A(ii, :) = [];
						b(ii) = [];
						Eqin(ii) = [];
						eliminatedRows = eliminatedRows + 1;
					end
				end
			end
        end
    end
	colindex = sort(colindex, 'descend');
    cardcols = nnz(colindex);
    if cardcols > 0 % delete variables
        for j = 1:cardcols
            A(:, colindex(j)) = [];
            c(colindex(j)) = [];
        end
    end
    if (eliminatedRows > 0) || (cardcols > 0)
        fprintf(['ELIMINATE KTON EQUALITY CONSTRAINTS: ' ... 
            '%i kton equality constraints were ' ...
            'found. %i constraints and %i variables were ' ...
            'eliminated\n'], cardcols, eliminatedRows, cardcols);
    end
end
end