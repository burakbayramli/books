function [A, c, b, Eqin, infeasible] = ...
	eliminateDualSingletonInequalityConstraints(A, c, b, Eqin)
% Filename: eliminateDualSingletonInequalityConstraints.m
% Description: the function is an implementation of the 
% presolve technique that eliminates dual singleton 
% inequality constraints
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, Eqin, infeasible] = ...
%	eliminateDualSingletonInequalityConstraints(A, c, b, Eqin)
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
% -- b: presolved vector of the right-hand side of the 
%    constraints (size m x 1)
% -- Eqin: presolved vector of the type of the constraints 
%    (size m x 1)
% -- infeasible: flag variable showing if the LP is 
%    infeasible or not
 
infeasible = 0;
% compute the number of nonzero elements in each column
delcols = sum(spones(A));
% find the columns that have only one nonzero element and set 
% all the other columns equal to zero
singleton = (delcols == 1);
% find the number of the columns that have only one nonzero 
% element
cardcols = nnz(singleton);
countercols = 0;
counterrows = 0;
if cardcols > 0 % if dual singleton constraints exist
	% get the indices of the dual singleton constraints
	idscols = int16(find(singleton));
	% compute the number of the dual singleton constraints
	cardcols = length(idscols);
	% for each dual singleton inequality constraint
	for j = cardcols:-1:1
		jj = idscols(j); % get the index of the constraint
        % find the nonzero elements of this column
		[row, ~] = find(A(:, jj));
		if ~isempty(row) % if there are nonzero elements
			if Eqin(row) == -1 % constraint type <=
				% eliminate the dual singleton inequality 
                % constraints (Case 1)
                if (A(row, jj) > 0) && (c(jj) > 0)
					A(:, jj) = [];
					c(jj) = [];
					countercols = countercols + 1;
				% check if the LP problem is infeasible
                % (Case 2)
                elseif (A(row, jj) < 0) && (c(jj) < 0)
					infeasible = 1;
					return;
                % eliminate the dual singleton inequality 
                % constraints (Case 3)
                elseif (A(row,jj) > 0) && (c(jj) == 0)
					A(:, jj) = [];
					c(jj) = [];
					countercols = countercols + 1;
                % eliminate the dual singleton inequality 
                % constraints (Case 4)
                elseif (A(row, jj) < 0) && (c(jj) == 0)
					A(:, jj) = [];
					c(jj) = [];
					A(row, :) = [];
					b(row) = [];
					Eqin(row) = [];
					countercols = countercols + 1;
					counterrows = counterrows + 1;
                end
            elseif Eqin(row) == 1 % constraint type >=
                % check if the LP problem is unbounded 
                % (Case 5)
                if (A(row, jj) > 0) && (c(jj) < 0)
					infeasible = 1;
					return;
                % eliminate the dual singleton inequality 
                % constraints (Case 6)
                elseif (A(row, jj) < 0) && (c(jj) > 0)
					A(:, jj) = [];
					c(jj) = [];
					countercols = countercols + 1;
                % eliminate the dual singleton inequality 
                % constraints (Case 7)
                elseif (A(row,jj) > 0) && (c(jj) == 0)
					A(:, jj) = [];
					c(jj) = [];
					A(row, :) = [];
					b(row) = [];
					Eqin(row) = [];
					countercols = countercols + 1;
					counterrows = counterrows + 1;
                % eliminate the dual singleton inequality 
                % constraints (Case 8)
                elseif (A(row, jj) < 0) && (c(jj) == 0)
					A(:, jj) = [];
					c(jj) = [];
					countercols = countercols + 1;
                end
			end
		end
	end
	if (countercols > 0) || (counterrows > 0)
		fprintf(['ELIMINATE DUAL SINGLETON INEQUALITY ' ... 
            'CONSTRAINTS: %i constraints and %i columns ' ... 
            'were eliminated\n'], counterrows, ... 
            countercols);
	end
end
end