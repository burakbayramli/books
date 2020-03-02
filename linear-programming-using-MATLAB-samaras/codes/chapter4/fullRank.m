function [A, b, Eqin] = fullRank(A, Atemp, b, Eqin)
% Filename: fullRank.m
% Description: the function is an implementation of the 
% presolve technique that makes the coefficient matrix 
% structurally full rank
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, b, Eqin] = fullRank(A, Atemp, b, Eqin)
% 
% Input:
% -- A: matrix of coefficients of the constraints before 
%    adding slack variables (size m x n)
% -- Atemp: matrix of coefficients of the constraints after
%    adding slack variables (depending on the type of the 
%    constraints: if all constraints are equalities then 
%    Atemp = A and the size of Atemp = m x n, if all 
%    constraints are inequalities then the size of Atemp
%    = m x (m + n))
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
 
sp_rank = sprank(Atemp'); % find the rank of matrix Atemp
[m, n] = size(Atemp); % size of Atemp
t = min(m, n);
if sp_rank < t % if the matrix is not of full rank
	[rmp, ~] = dmperm(Atemp);
	rows = rmp(1:sp_rank);
	A = A(rows, :);
	b = b(rows);
	Eqin = Eqin(rows);
	fprintf(['MAKE COEFFICIENT MATRIX STRUCTURALLY FULL ' ...
        'RANK: %i rows were eliminated\n'], t - sp_rank);
end
end