function [A, c, b, row_multi, col_multi] = ...
    lpnorminf(A, c, b)
% Filename: lpnorminf.m
% Description: the function is an implementation of the 
% Lp-norm for the case p = inf scaling technique
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, row_multi, col_multi] = ... 
%   lpnorminf(A, c, b)
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
row_max = zeros(m, 1);
row_min = zeros(m, 1);
row_multi = zeros(m, 1);
for i = 1:m
	% find the indices of the nonzero elements of the 
	% specific row
	ind = find(A(i, :));
	% if the specific row contains at least one nonzero 
	% element
	if ~isempty(ind)
        % find the maximum in absolute value of the nonzero 
        % elements of the specific row
        row_max(i) = max(max(abs(A(i, ind))));
        % find the minimum in absolute value of the nonzero 
        % elements of the specific row
        row_min(i) = min(min(abs(A(i, ind))));
        % calculate the specific row scaling factor
        row_multi(i) = 1 / (sqrt(row_max(i) * row_min(i)));
        % scale the elements of the specific row of matrix A
        A(i, :) = A(i, :) * row_multi(i);
        % scale the elements of vector b
        b(i) = b(i) * row_multi(i);
	end
end
% then apply column scaling
col_max = zeros(n, 1);
col_min = zeros(n, 1);
col_multi = zeros(n, 1);
for j = 1:n
	% find the indices of the nonzero elements of the 
	% specific column
	ind = find(A(:, j));
	% if the specific column contains at least one nonzero 
	% element
	if ~isempty(ind)
        % find the maximum in absolute value of the nonzero 
        % elements of the specific column
        col_max(j) = max(max(abs(A(ind, j))));
        % find the minimum in absolute value of the nonzero 
        % elements of the specific column
        col_min(j) = min(min(abs(A(ind, j))));
        % calculate the specific column scaling factor
        col_multi(j) = 1 / (sqrt(col_max(j) * col_min(j)));
        % scale the elements of the specific column of 
        % matrix A
        A(:, j) = A(:, j) * col_multi(j);
        % scale the elements of vector c
        c(j) = c(j) * col_multi(j);
	end
end
end