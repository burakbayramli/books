function [A, c, b, row_multi, col_multi] = lpnorm2(A, c, b)
% Filename: lpnorm2.m
% Description: the function is an implementation of the 
% Lp-norm for the case p = 2 scaling technique
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, row_multi, col_multi] = lpnorm2(A, c, b)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- c: vector of coefficients of the objective function 
%    (size n x 1)
% -- b: vector of the right-hand side of the constraints 
%    (size m x 1)
%
% Output:
% -- A: scaled matrix of coefficients of the constraints 
%    (size m x n)
% -- c: scaled vector of coefficients of the objective 
%    function (size n x 1)
% -- b: scaled vector of the right-hand side of the 
%    constraints (size m x 1)
% -- row_multi: vector of the row scaling factors 
%    (size m x 1)
% -- col_multi: vector of the column scaling factors 
%    (size n x 1)

[m, n] = size(A); % size of matrix A
% first apply row scaling
row_prod = zeros(m, 1);
row_multi = zeros(m, 1);
for i = 1:m
	% find the indices of the nonzero elements of the 
	% specific row
	ind = find(A(i, :));
	% if the specific row contains at least one nonzero 
	% element
	if ~isempty(ind)
        % find the inverse of the product in absolute 
        % value of the nonzero elements of the specific 
        % row
        row_prod(i) = 1 / prod(abs(A(i, ind)))^ ...
            (-1 / length(ind));
        % calculate the specific row scaling factor
        row_multi(i) = 1 / row_prod(i);
        % scale the elements of the specific row of matrix A
        A(i, :) = A(i, :) * row_multi(i);
        % scale the elements of vector b
        b(i) = b(i) * row_multi(i);
	end
end
% then apply column scaling
col_prod = zeros(n, 1);
col_multi = zeros(n, 1);
for j = 1:n
	% find the indices of the nonzero elements of the 
	% specific row
	ind = find(A(:, j));
	% if the specific column contains at least one nonzero 
	% element
	if ~isempty(ind)
        % find the inverse of the product in absolute value 
        % of the nonzero elements of the specific column
        col_prod(j) = 1 / prod(abs(A(ind, j)))^ ...
            (-1 / length(ind));
        % calculate the specific column scaling factor
        col_multi(j) = 1 / col_prod(j);
        % scale the elements of the specific column of 
        % matrix A
        A(:, j) = A(:, j) * col_multi(j);
        % scale the elements of vector c
        c(j) = c(j) * col_multi(j);
	end
end
end