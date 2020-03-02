function [A, c, b, Eqin, infeasible] = ...
	eliminateSingletonInequalityConstraints(A, c, b, Eqin)
% Filename: eliminateSingletonInequalityConstraints.m
% Description: the function is an implementation of the 
% presolve technique that eliminates singleton 
% inequality constraints
% Authors: Ploskas, N., & Samaras, N.
% 
% Syntax: [A, c, b, Eqin, infeasible] = ...
%	eliminateSingletonInequalityConstraints(A, c, b, Eqin)
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
% compute the number of nonzero elements in each row
delrows = sum(spones(A'));
% find the rows that have only one nonzero element and set 
% all the other rows equal to zero
singleton = (delrows == 1);
% find the number of the rows that have only one nonzero 
% element
cardrows = nnz(singleton);
counterrows = 0;
countercols = 0;
if cardrows > 0 % if singleton rows exist
	% get the indices of the singleton rows
	idsrows = int16(find(singleton));
	% find inequality constraints
	[row, ~] = find(Eqin ~= 0);
	% compute the indices of the singleton inequality 
	% constraints
	idsrows = intersect(idsrows, row);
	% compute the number of the singleton inequality 
	% constraints
	cardrows = length(idsrows);
	% for each singleton inequality constraint
	for i = cardrows:-1:1
        ii = idsrows(i); % get the index of the constraint
        % find the nonzero elements of this row
        [row1, j] = find(A(ii, :));
        if ~isempty(row1) % if there are nonzero elements
            if Eqin(ii) == -1 % constraint type <=
                % check if the LP problem is infeasible 
                % (Case 1)
                if (A(ii, j) > 0) && (b(ii) < 0)
					infeasible = 1;
					return;
                % eliminate the singleton inequality constraints 
                % (Case 2)
                elseif (A(ii, j) < 0) && (b(ii) > 0)
					A(ii, :) = [];
					b(ii) = [];
					Eqin(ii) = [];
                    counterrows = counterrows + 1;
                % eliminate the singleton inequality constraints 
                % (Case 3)
                elseif (A(ii, j) > 0) && (b(ii) == 0)
					A(ii, :) = [];
					b(ii) = [];
					Eqin(ii) = [];
					c(j) = [];
					A(:, j) = [];
                    counterrows = counterrows + 1;
                    countercols = countercols + 1;
                % eliminate the singleton inequality constraints 
                % (Case 4)
                elseif (A(ii, j) < 0) && (b(ii) == 0)
					A(ii, :) = [];
					b(ii) = [];
					Eqin(ii) = [];
                    counterrows = counterrows + 1;
                end
            elseif Eqin(ii) == 1 % constraint type >=
                % eliminate the singleton inequality constraints 
                % (Case 5)
                if((A(ii, j) > 0) && (b(ii) < 0))
					A(ii, :) = [];
					b(ii) = [];
					Eqin(ii) = [];
                    counterrows = counterrows + 1;
                % check if the LP problem is infeasible 
                % (Case 6)
                elseif (A(ii, j) < 0) && (b(ii) > 0)
					infeasible = 1;
					return;
                % eliminate the singleton inequality constraints 
                % (Case 7)
                elseif (A(ii, j) > 0) && (b(ii) == 0)
					A(ii, :) = [];
					b(ii) = [];
					Eqin(ii) = [];
                    counterrows = counterrows + 1;
                % eliminate the singleton inequality constraints 
                % (Case 8)
                elseif (A(ii,j) < 0) && (b(ii) == 0)
					A(ii, :) = [];
					b(ii) = [];
					Eqin(ii) = [];
					c(j) = [];
					A(:, j) = [];
                    counterrows = counterrows + 1;
                    countercols = countercols + 1;
                end
            end
        end
	end
    if (counterrows ~= 0) || (countercols ~= 0)
        fprintf(['ELIMINATE SINGLETON INEQUALITY CONSTRAINTS: ' ... 
            '%i singleton inequality constraints were found. ' ... 
            '%i constraints and %i variables were eliminated ' ...
            '\n'], cardrows, counterrows, countercols);
    end
end
end