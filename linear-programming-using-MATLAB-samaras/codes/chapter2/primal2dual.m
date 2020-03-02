function [DA, Dc, Db, DEqin, DMinMaxLP, DVarConst] = ...
    primal2dual(A, c, b, Eqin, MinMaxLP, VarConst)
% Filename: primal2dual.m
% Description: the function is an implementation of the 
% transformation of a primal LP problem to its dual
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [DA, Dc, Db, DEqin, DMinMaxLP, DVarConst] = ...
%   primal2dual(A, c, b, Eqin, MinMaxLP, VarConst)
% 
% Input:
% -- A: matrix of coefficients of the constraints of the
%    primal LP problem (size m x n)
% -- c: vector of coefficients of the objective function 
%    of the primal LP problem (size n x 1)
% -- b: vector of the right-hand side of the constraints 
%    of the primal LP problem (size m x 1)
% -- Eqin: vector of the type of the constraints of the
%    primal LP problem (size m x 1)
% -- MinMaxLP: the type of optimization of the primal 
%    LP problem
% -- VarConst: vector of the variables' constraints of the
%    primal LP problem (0 - free variable, 1 - variable 
%    <= 0, 2 - variable >= 0) (size n x 1)
%
% Output:
% -- DA: matrix of coefficients of the constraints of the
%    dual LP problem (size n x m)
% -- Dc: vector of coefficients of the objective function 
%    of the dual LP problem (size m x 1)
% -- Db: vector of the right-hand side of the constraints 
%    of the dual LP problem (size n x 1)
% -- DEqin: vector of the type of the constraints of the
%    dual LP problem (size n x 1)
% -- DMinMaxLP: the type of optimization of the dual 
%    LP problem
% -- DVarConst: vector of the variables' constraints of the
%    dual LP problem (0 - free variable, 1 - variable 
%    <= 0, 2 - variable >= 0) (size m x 1)

% if the primal is a minimization problem, then the
% dual is a maximization one and vice versa
DMinMaxLP = -MinMaxLP;
% the coefficients of the objective function of the 
% dual LP problem are the right-hand side values of 
% the primal LP problem
Dc = b;
% the right-hand side values of the dual LP problem 
% are the coefficients of the objective function of the 
% primal LP problem
Db = c;
% the coefficient matrix of the dual LP problem is the 
% transpose of the coefficient matrix of the primal LP 
% problem
DA = A';
[m, n] = size(A); % size of matrix A
% calculate the vector of the type of the constraints
DEqin = zeros(n, 1);
for i = 1:n
    % free variable -> equality constraint
	if VarConst(i) == 0
        DEqin(i) = 0;
    % variable <= 0 -> 'greater than or equal to'
    % constraint
    elseif VarConst(i) == 1
        DEqin(i) = 1;
    % variable >= 0 -> 'less than or equal to'
    % constraint
	elseif VarConst(i) == 2
        DEqin(i) = -1;
	end
end
% calculate the vector of variables' constraints
DVarConst = zeros(m, 1);
for j = 1:m
    % equality constraint -> free variable
	if Eqin(j) == 0
        DVarConst(j) = 0;
    % 'less than or equal to' constraint -> 
    % variable <= 0
    elseif Eqin(j) == -1
        DVarConst(j) = 1;
    % 'greater than or equal to' constraint -> 
    % variable >= 0
	elseif Eqin(j) == 1
        DVarConst(j) = 2;
	end
end
end