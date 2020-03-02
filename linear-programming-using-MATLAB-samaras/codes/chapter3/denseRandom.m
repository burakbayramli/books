function [A, c, b, Eqin, MinMaxLP]= denseRandom(m, n, ...
    optType, Alu, clu, blu, Eqinlu)
% Filename: denseRandom.m
% Description: Creates dense random LPs
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, MinMaxLP, Eqin]= denseRandom(m, n, ...
%    minCard, Alu, clu, blu, Eqinlu)
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
A = floor((Au - Al + 1) * rand(m, n)) + Al;
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