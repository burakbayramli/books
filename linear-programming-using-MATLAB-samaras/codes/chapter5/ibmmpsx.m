function [A, c, b, row_multigm, col_multigm, ... 
    row_multieq, col_multieq] = ibmmpsx(A, c, b)
% Filename: ibmmpsx.m
% Description: the function is an implementation of the IBM 
% MPSX scaling technique
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, row_multigm, col_multigm, ...
%   row_multieq, col_multieq] = ibmmpsx(A, c, b)
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
% -- row_multigm: vector of the row scaling factors from 
%    geometric mean scaling technique (size m x 1)
% -- col_multigm: vector of the column scaling factors from 
%    geometric mean scaling technique (size n x 1)
% -- row_multieq: vector of the row scaling factors from 
%    equilibration scaling technique (size m x 1)
% -- col_multieq: vector of the column scaling factors from 
%    equilibration scaling technique (size n x 1)

[m, n] = size(A); % size of matrix A
row_multigm = zeros(m, 1);
col_multigm = zeros(n, 1);
row_multieq = zeros(m, 1);
col_multieq = zeros(n, 1);
max_A = max(max(A)); % find the maximum element of matrix A
min_A = min(min(A)); % find the minimum element of matrix A
tol = max(m, n) * eps * norm(A, 'inf');
tole = 10; % initialize tolerance
% if the absolute value of the difference of the maximum 
% element and the minimum element are below tol, then do not 
% scale
if abs(max_A - min_A) > tol
	counter = 0;
	% geometric mean is performed four times
	while(counter < 4)
        [A, c, b, row_multigm, col_multigm] = ...
            geometricMean(A, c, b);
        % or until the following relation holds true
        if((1 / nnz(A)) * (sum(sum(power(A, 2))) - ...
                power(sum(sum(abs(A))), 2) / nnz(A)) < tole)
            break;
        end
        counter = counter + 1;
	end
	% then equilibration scaling technique is performed
	[A, c, b, row_multieq, col_multieq] = ...
        equilibration(A, c, b);
end
end