function [A, c, b, Eqin, MinMaxLP] = ...
    sparseRandomOptimal(m, n, optType, Alu, clu, blu, ...
    Eqinlu, dense, t1, t2)
% Filename: sparseRandomOptimal.m
% Description: Creates sparse random optimal LPs
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, Eqin, MinMaxLP] = ...
%     sparseRandomOptimal(m, n, optType, Alu, clu, blu, ...
%     Eqinlu, dense, t1, t2)
%
% Input:
% -- m: the number of constraints of the generated random 
%    LPs
% -- n: the number of variables of the generated random 
%    LPs
% -- optType: the type of optimization (0 min, 1 max, 2 
%    random)
% -- Alu: the ranges of the values for matrix A 
%    (size 1 x 2)
% -- clu: the ranges of the values for vector c 
%    (size 1 x 2)
% -- blu: the ranges of the values for vector b 
%    (size 1 x 2)
% -- Eqinlu: the ranges of the values for vector Eqin 
%    (size 1 x 2)
% -- dense: the density of matrix A
% -- t1: the lower bound of b1
% -- t2: the upper bound of b1
%
% Output:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- c: vector of coefficients of the objective function
%   (size n x 1)
% -- b: vector of the right-hand side of the constraints
%   (size m x 1)
% -- Eqin: vector of the type of the constraints 
%    (size m x 1)
% -- MinMaxLP: the type of optimization

Al = Alu(1); % get the lower bound of A
Au = Alu(2); % get the upper bound of A
% create matrix A
A = round((Au - Al + 1) * sprand(m, n, dense));
[k, l, s] = find(A);
for i = 1:length(k)
	A(k(i), l(i)) = s(i) + Al;
end
cl = clu(1); % get the lower bound of c
cu = clu(2); % get the upper bound of c
% create vector c
c = floor((cu - cl + 1) * rand(n, 1)) + cl;
bl = blu(1); % get the upper bound of b
bu = blu(2); % get the upper bound of c
% create vector b
b = floor((bu - bl + 1) * rand(m, 1)) + bl;
eqinl = Eqinlu(1); % get the lower bound of Eqin
eqinu = Eqinlu(2); % get the upper bound of Eqin
% create vector Eqin
Eqin = floor((eqinu - eqinl + 1) * rand(m, 1)) + eqinl;
if any(Eqin == 1) % increase the value of the less than or 
	% equal to constraints in order to create artificially 
	% a closed polyhedron
	m_1 = find(Eqin == 1);
	m_11 = length(m_1);
	b(m_1) = floor((t2 - t1 + 1) * rand(m_11, 1)) + t1;
end
% less than or equal to inequality constraints
Eqin(Eqin == 1) = -1;
% greater than or equal to inequality constraints
Eqin(Eqin == 2) = 1;
if optType == 0 % minimization
	MinMaxLP = -1;
elseif optType == 1 % maximization
	MinMaxLP = 1;
else % pick random optimization type
	MinMaxLP = -1;
	if randi(2) < 2
        MinMaxLP = 1;
	end
end
end