function sparseMenu(m, n)
% Filename: sparseMenu.m
% Description: Custom menu for creating sparse random LPs
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: sparseMenu(m, n)
%
% Input:
% -- m: the number of constraints of the generated random 
%    LPs
% -- n: the number of variables of the generated random 
%    LPs
%
% Output: files that store the generated sparse random LPs

% read the directory to store the LPs
pathName = input(['Please give the path where you ' ...
    'want to store the generated LPs: ']);
% read the number of the LPs to generate
nOfProblems = input(['Please give the number of the ' ...
    'LPs that you want to create:  ']); 
% read the density of the LPs to generate
dense = input('Please enter the density of the LP:  ');
% read the ranges of the values for matrix A
Alu = input(['Please give the range of the values for ' ...
    'matrix A: ']);
% read the ranges of the values for vector c
clu = input(['Please give the range of the values for ' ...
    'vector c: ']);
% read the ranges of the values for vector b
blu = input(['Please give the range of the values for ' ...
    'vector b: ']);
% read the ranges of the values for vector Eqin
Eqinlu = input(['Please give the range of the values ' ...
    'for vector Eqin (0 - equality constraints, 1 - ' ...
    'less than or equal to inequality constraints, ' ...
    '2 - greater than or equal to inequality ' ...
    'constraints: ']);
% read the type of optimization
optType = input(['Please give the type of ' ...
    'optimization (0 min, 1 max, 2 random): ']);
% read if all LPs will be optimal
optimality = input(['Do you want all the LPs to be ' ...
    'optimal (1 yes, 2 no): ']);
if optimality == 1 % optimal LPs
	% read the lower bound of b1
	t1 = input('Please give the lower bound of b1: ');
	% read the upper bound of b1
	t2 = input('Please give the upper bound of b1: ');
	% create nOfProblems sparse random optimal LPs
	for i = 1:nOfProblems
        [A, c, b, Eqin, MinMaxLP] = ...
            sparseRandomOptimal(m, n, optType, Alu, ... 
            clu, blu, Eqinlu, dense, t1, t2);
        s1 = num2str(m);
        s2 = num2str(n);
        k = num2str(i);
        fname = [pathName '/' 'sdata' k '_' s1 'x' s2];
        Name = ['sdata' k '_' s1 'x' s2];
        A = sparse(A);
        c = sparse(c);
        b = sparse(b);
        Eqin = sparse(Eqin);
        R = [];
        BS = [];
        NonZeros = nnz(A);
        c0 = 0;
        c00 = 0;
        eval (['save ' fname '.mat A c b Eqin MinMaxLP ' ...
            'Name R BS NonZeros c0 c00']);
    end
else % not optimal LPs
	% create nOfProblems sparse random LPs
	for i = 1:nOfProblems
        [A, c, b, Eqin, MinMaxLP] = sparseRandom(m, n, ...
            optType, Alu, clu, blu, Eqinlu, dense);
        s1 = num2str(m);
        s2 = num2str(n);
        k = num2str(i);
        fname = [pathName '/' 'sdata' k '_' s1 'x' s2];
        Name = ['sdata' k '_' s1 'x' s2];
        A = sparse(A);
        c = sparse(c);
        b = sparse(b);
        Eqin = sparse(Eqin);
        R = [];
        BS = [];
        NonZeros = nnz(A);
        c0 = 0;
        c00 = 0;
        eval (['save ' fname '.mat A c b Eqin MinMaxLP ' ...
            ' Name R BS NonZeros c0 c00']);
	end
end
end