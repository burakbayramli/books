function [A, b, Eqin, infeasible] = ...
    eliminateRedundantRows(A, b, Eqin)
% Filename: eliminateRedundantRows.m
% Description: the function is an implementation of the 
% presolve technique that eliminates redundant rows
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, b, Eqin, infeasible] = ...
%   eliminateRedundantRows(A, b, Eqin)
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
% -- A: presolved matrix of coefficients of the 
%    constraints (size m x n)
% -- b: presolved vector of the right-hand side of the 
%    constraints (size m x 1)
% -- Eqin: presolved vector of the type of the 
%    constraints (size m x 1)
% -- infeasible: flag variable showing if the LP problem 
%    is infeasible or not
 
infeasible = 0;
row = find(Eqin == 0); % find the equality constraints
% get the rows of A that correspond to equality 
% constraints
Aeqin = A(row, :);
% get the elements of b that correspond to equality 
% constraints
beqin = b(row, :);
% find the number of the equality constraints
[m1, n] = size(Aeqin);
% create a 2 x m1 zero matrix
rowindex = zeros(2, m1);
% store the indices of the equality constraints in the 
% first row
rowindex(1, 1:m1) = row';
% compute the tolerance
tol = max(m1, n) * eps * norm(A, 'inf');
i = 1; 
j = 1;
% perform row operations using Gauss - Jordan elimination 
% with partial pivoting
while (i <= m1) && (j <= n)
	% find the maximum element of jth column of Aeqin in 
	% absolute value
	[p, k] = max(abs(Aeqin(i:m1, j))); 
	% if the maximum element is less than tol, then set 
	% all the elements of jth column equal to zero
	if p < tol
        Aeqin(i:m1, j) = zeros(m1 - i + 1, 1);
        j = j + 1;
	% perform the pivoting operation and update matrix 
	% Aeqin and vector beqin
	else
        if p ~= 0
            k = k + i - 1;
            rowindex(:, [i k]) = rowindex(:, [k i]);
            beqin(i, :) = beqin(i, :) / Aeqin(i, j);
            Aeqin(i, j:n) = Aeqin(i, j:n) / Aeqin(i, j);
            i_nz = find(Aeqin(:, j));
            i_nz = setdiff(i_nz, i);
            for t = i_nz
                if beqin(i) ~= 0
                    beqin(t) = beqin(t) - ...
                        Aeqin(t, j) * beqin(i);
                    toler = abs(beqin) <= tol;
                    beqin(toler == 1) = 0;
                end
                Aeqin(t, j:n) = Aeqin(t, j:n) ...
                    - Aeqin(t, j) * Aeqin(i, j:n);
                toler = abs(Aeqin) <= tol;
                Aeqin(toler == 1) = 0;
            end
            i = i + 1;
            j = j + 1;
        end
	end
end
i = 1;
for h = [1:i - 1 i + 1:m1]
	% if all elements of row h are equal to zero and the 
	% corresponding right-hand side is equal to 0, then 
	% row h is redundant and can be deleted
	if (Aeqin(h, :) == 0) & (beqin(h) == 0)
		rowindex(2, h) = 1;
	end
	% if all elements of row h are equal to zero and the 
	% corresponding right-hand side is not equal to 0, 
	% then the LP problem is infeasible
	if (Aeqin(h, :) == 0) & (beqin(h) ~= 0)
		infeasible = 1;
		return;
	end
end
% find the rows that have been marked with 1
if any(rowindex(2, :) == 1)
	row = find(rowindex(2, :) == 1);
end
row = (rowindex(2, :) == 1);
row = rowindex(1, row);
row = sort(row);
% find the number of the rows that have been marked with 1
cardrows = length(row);
if cardrows > 0 % if such rows exist
	% eliminate these rows and update matrix A and vectors
	% b and Eqin
	for i = cardrows:-1:1
		ii = row(i);
		A(ii, :) = [];
		b(ii) = [];
		Eqin(ii) = [];
	end
	fprintf(['ELIMINATE REDUNDANT ROWS: %i redundant ' ...
        'rows were eliminated\n'], cardrows);
end
end