function [A, c, b, Eqin, c0] = ...
	eliminateImpliedFreeSingletonColumns(A, c, b, Eqin, c0)
% Filename: eliminateImpliedFreeSingletonColumns.m
% Description: the function is an implementation of the 
% presolve technique that eliminates implied free 
% singleton constraints
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, Eqin, c0] = ...
%	eliminateImpliedFreeSingletonColumns(A, c, b, Eqin, c0)
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
%   (size m x n)
% -- c: presolved vector of coefficients of the objective 
%    function (size n x 1)
% -- b: presolved vector of the right-hand side of the 
%    constraints (size m x 1)
% -- Eqin: presolved vector of the type of the constraints 
%    (size m x 1)
% -- c0: updated constant term of the objective function
 
% compute the number of nonzero elements in each column
delcols = sum(spones(A));
% find the columns that have only one nonzero element and 
% set all the other columns equal to zero
singleton = (delcols == 1);
% find the number of the columns that have only one nonzero 
% element
cardcols = nnz(singleton);
idscols = find(singleton);
countercols = 0;
for j = cardcols:-1:1 % for each dual singleton constraint
	jj = idscols(j); % get the index of the constraint
	% find the nonzero elements of this column
	[row, ~] = find(A(:, jj));
	if ~isempty(row) % if there are nonzero elements
		if Eqin(row) == 0 % constraint type =
            % if the element is greater than zero
            if A(row, jj) > 0
                % find the number of columns
                n0 = size(A, 2);
				% compute the indices of all other columns
				set = setdiff(1:n0, jj);
				% if all the elements of the row except 
				% the one in the specific column are less 
				% than or equal to zero and the right-hand 
				% side greater than or equal to zero,
				% then update matrix A, vectors c, b, and 
				% Eqin, and variable c0
                if all(A(row, set) <= 0) && b(row) >= 0
					if c(jj) ~= 0
						c0 = c0 + c(jj) * (b(row) / A(row, jj));
						c = c - (c(jj) / A(row, jj)) * A(row, :)';
					end
					c(jj) = [];
					A(:, jj) = [];
					A(row, :) = [];
					b(row) = [];
					Eqin(row) = [];
					countercols = countercols + 1;
                end
            elseif A(row, jj) < 0 % if the element is less 
				% than zero
				n0 = size(A, 2); % find the number of columns
				% compute the indices of all other columns
				set = setdiff(1:n0, jj);
				% if all the elements of the row except the 
				% one in the specific column are greater than 
				% or equal to zero and the right-hand side 
				% less than or equal to zero, then update 
				% matrix A, vectors c, b, and Eqin, and
				% variable c0
				if all(A(row, set) >= 0) && b(row) <= 0
					if c(jj) ~= 0
						c0 = c0 + c(jj) * (b(row) / A(row, jj));
						c = c - (c(jj) / A(row,jj)) * A(row, :)';
					end
					c(jj) = [];
					A(:, jj) = [];
					A(row, :) = [];
					b(row) = [];
					Eqin(row) = [];
					countercols = countercols + 1;
				end
            end
        elseif Eqin(row) == 1 % constraint type <=
            if A(row, jj) > 0 % if the element is greater 
				% than zero
				n0 = size(A, 2); % find the number of columns
				% compute the indices of all other columns
				set = setdiff(1:n0, jj);
				% if all the elements of the row except 
				% the one in the specific column are less 
				% than or equal to zero and the right-hand 
				% side greater than or equal to -1, then 
				% update matrix A, vectors c, b, and Eqin, and
				% variable c0
				if all(A(row, set) <= 0) && b(row) >= -1
					if c(jj) ~= 0
						c0 = c0 + c(jj) * (b(row) ...
                            / A(row, jj));
						c = c - (c(jj) / A(row, jj)) ...
                            * A(row, :)';
					end
					c(jj) = [];
					A(:, jj) = [];
					A(row, :) = [];
					b(row) = [];
					Eqin(row) = [];
					countercols = countercols + 1;
				end
            end
        elseif Eqin(row) == -1 % constraint type >=
			if A(row, jj) < 0 % if the element is less 
				% than zero
				n0 = size(A, 2); % find the number of columns
				% compute the indices of all other columns
				set = setdiff(1:n0, jj);
				% if all the elements of the row except 
				% the one in the specific column are greater 
				% than or equal to zero and the right-hand 
				% side less than or equal to 1, then update 
				% matrix A, vectors c, b, and Eqin, and
				% variable c0
				if all(A(row, set) >= 0) && b(row) <= 1
					if c(jj) ~= 0
						c0 = c0 + c(jj) * (b(row) ...
                            / A(row, jj));
						c = c - (c(jj) / A(row, jj)) ...
                            * A(row, :)';
					end
					c(jj) = [];
					A(:, jj) = [];
					A(row, :) = [];
					b(row) = [];
					Eqin(row) = [];
					countercols = countercols + 1;
				end
			end		
		end
	end
end
if countercols > 0
	fprintf(['ELIMINATE IMPLIED FREE SINGLETON COLUMNS: ' ...
        '%i rows and %i columns were eliminated\n'], ...
        countercols, countercols);
end
end