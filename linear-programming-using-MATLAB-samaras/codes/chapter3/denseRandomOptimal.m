function [A, c, b, Eqin, MinMaxLP] = ...
    denseRandomOptimal(m, n, optType, Alu, clu, center, R)
% Filename: denseRandomOptimal.m
% Description: Creates dense random optimal LPs
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, Eqin, MinMaxLP] = ...
%    denseRandomOptimal(m, n, optType, Alu, clu, center, R)
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
% -- center: the center of the circle
% -- R: the radius of the circle

% Output:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- c: vector of coefficients of the objective function
%    (size n x 1)
% -- b: vector of the right-hand side of the constraints
%    (size m x 1)
% -- Eqin: vector of the type of the constraints 
%    (size m x 1)
% -- MinMaxLP: the type of optimization
%
cl = clu(1); % get the lower bound of c
cu = clu(2); % get the upper bound of c
% create vector c
c = round((cu - cl + 1) * randn(n, 1)) + cl;
Al = Alu(1); % get the lower bound of A
Au = Alu(2); % get the upper bound of A
A = zeros(m, n);
b = zeros(m, 1);
k = ones(1, n) * center;
for i = 1:m % create matrix A and vector b
    % create a row of matrix A
	a = round((Au - Al + 1) * rand(1, n)) + Al;
    % calculate the hyperplane that is tangent 
    % in a random point of the sphere
	y = k + r * (a / norm(a));
	b0 = a * y';
	b(i, 1) = b0; % add the point to vector b
	A(i, :) = a; % add the row to matrix A
end
% create vector Eqin
Eqin(1:m, 1) = -1;
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