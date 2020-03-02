function [cl, cu] = sensitivityAnalysisc(A, c, BasicList)
% Filename: sensitivityAnalysisc.m
% Description: the function is an implementation of the 
% sensitivity analysis for the coefficients of the 
% objective function
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [cl, cu, sp] = sensitivityAnalysisc(A, c, ...
%   MinMaxLP, BasicList)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- c: vector of coefficients of the objective function 
%    (size n x 1)
% -- BasicList: vector of the indices of the basic 
%    variables (size 1 x m)
%
% Output:
% -- cl: vector of the lower values of the range of the 
%    coefficients of the objective function (size 1 x n)
% -- cu: vector of the upper values of the range of the 
%    coefficients of the objective function (size 1 x n)

n = length(c); % find the number of variables
% initialize output
cl = zeros(1, n);
cu = zeros(1, n);
% find the nonbasic list
NonBasicList = setdiff([1:n], BasicList);
BasisInv = inv(A(:, BasicList)); % invert the basis
w = c(BasicList)' * BasisInv; % calculate the simplex multiplier
% calculate the reduced costs
Sn = c(NonBasicList)' - w * A(:, NonBasicList);
% calculate the lower and upper values of the range 
% of the coefficients of the objective function
for j = 1:n
	% if it is a nonbasic variable
	if ismember(j, NonBasicList)
        t = find(NonBasicList == j);
        cu(j) = Inf;
        cl(j) = c(j) - Sn(t(1));
	else % if it is a basic variable
        r = find(BasicList == j);
        HrN = BasisInv(r, :) * A(:, NonBasicList);
        HrNplus = find(HrN > 0);
        HrNminus = find(HrN < 0);
        if isempty(HrNplus)
            cu(j) = Inf;
        else
            cu(j) = c(j) + min(Sn(HrNplus) ...
                ./ HrN(HrNplus));
        end
        if isempty(HrNminus)
            cl(j) = -Inf;
        else
            cl(j) = c(j) + max(Sn(HrNminus) ...
                ./ HrN(HrNminus));
        end
    end
end
end