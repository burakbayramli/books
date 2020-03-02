function [cBounds, bBounds, sp, rc] = ...
    sensitivityAnalysis(A, c, b, Eqin, MinMaxLP, BasicList)
% Filename: sensitivityAnalysis.m
% Description: the function is an implementation of the 
% sensitivity analysis
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [cBounds, bBounds, sp, rc] = ...
%   sensitivityAnalysis(A, c, b, Eqin, MinMaxLP, BasicList)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- c: vector of coefficients of the objective function 
%    (size n x 1)
% -- b: vector of the right-hand side of the constraints 
%    (size m x 1)
% -- Eqin: vector of the type of the constraints 
%    (size m x 1)
% -- MinMaxLP: the type of optimization (optional: 
%    default value -1 - minimization)
% -- BasicList: vector of the indices of the basic 
%    variables (size 1 x m)
%
% Output:
% -- cBounds: matrix of the range of the coefficients
%    of the objective function (size 2 x n)
% -- bBounds: matrix of the range of the right-hand
%    side coefficients (size 2 x m)
% -- sp: vector of the shadow prices (size 1 x m)
% -- rc: vector of the reduced costs (size 1 x n)

% transform the LP problem in its standard form
[A, c, b, Eqin, MinMaxLP] = ...
    general2standard(A, c, b, Eqin, MinMaxLP);
% find the ranges of the coefficients of the 
% objective function
[cl, cu] = sensitivityAnalysisc(A, c, BasicList);
cBounds = [cl; cu];
% find the ranges of the right-hand side 
% coefficients
[bl, bu] = sensitivityAnalysisb(A, b, BasicList);
bBounds = [bl; bu];
% calculate the shadow prices and reduced costs
BasisInv = inv(A(:, BasicList)); % invert the basis
% calculate the reduced costs
sp = c(BasicList)' * BasisInv;
% calculate the shadow prices
rc = c' - sp * A;
end