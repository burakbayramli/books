function [AEqin, bEqin, jb, out, rowindex, ...
    infeasible] = lprref(A, b, Eqin, tol)
% Filename: lprref.m
% Description: the function calculates a row echelon
% form of matrix A for LPs using Gauss-Jordan 
% elimination with partial pivoting
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [AEqin, bEqin, jb, out, rowindex, ...
%   infeasible] = lprref(A, b, Eqin, tol)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- b: vector of the right-hand side of the constraints 
%    (size m x 1)
% -- Eqin: vector of the type of the constraints 
%    (size m x 1)
% -- tol: tolerance
%
% Output:
% -- AEqin: reduced row echelon form of A (size m x n)
% -- bEqin: reduced vector b (size m x 1)
% -- jb: basis of matrix A (size m x m)
% -- rowindex: a matrix with the row redundant indices
%    (size 2 x m)
% -- infeasible: if the problem is infeasible or not

% initialize output variables
infeasible = 0;
out = [];
% find all equality constraints
a0 = find(Eqin == 0);
AEqin = A(a0, :);
bEqin = b(a0, :);
[m, n] = size(AEqin);
rowindex = zeros(2, m);
rowindex(1, 1:m) = a0';
if nargin < 4 % compute tol, if it was not given as input
	tol = max(m, n) * eps * norm(A, 'inf');
end
i = 1;
j = 1;
jb = [];
% apply Gauss-Jordan elimination with partial pivoting
while (i <= m) && (j <= n)
	[p, k] = max(abs(AEqin(i:m, j)));
	if p < tol
        AEqin(i:m, j) = zeros(m - i + 1, 1);
        j = j + 1;
	elseif p ~= 0
        k = k + i - 1;
        jb = [jb j];
        AEqin([i k], :) = AEqin([k i], :);
        bEqin([i k], :) = bEqin([k i], :);
        rowindex(:, [i k]) = rowindex(:, [k i]);
        bEqin(i, :) = bEqin(i, :) / AEqin(i, j);
        AEqin(i, j:n) = AEqin(i, j:n) / AEqin(i, j);
        i_nz = find(AEqin(:, j));
        i_nz = setdiff(i_nz, i);
        for t = i_nz
            if bEqin(i) ~= 0
                bEqin(t) = bEqin(t) - AEqin(t, j) * bEqin(i);
                toler = abs(bEqin) <= tol;
                bEqin(toler == 1) = 0;
            end
            AEqin(t, j:n) = AEqin(t, j:n) - ...
                AEqin(t, j) * AEqin(i, j:n);
            toler = abs(AEqin) <= tol;
            AEqin(toler == 1) = 0;
        end
        i = i + 1;
        j = j + 1;
	end
end
% check for redundant and infeasible constraints
i = 1;
for h = [1:i - 1 i + 1:m]
    % redundant constraint
	if (AEqin(h, :) == 0) & (bEqin(h) == 0)
        rowindex(2, h) = 1;
	end
	% infeasible constraint
	if (AEqin(h,:) == 0) & (bEqin(h) ~= 0)
        infeasible = 1;
        return;
	end
end
% find the indices of the redundant constraints
if any(rowindex(2, :) == 1)
	y = find(rowindex(2, :) == 1);
	out = y;
end
end