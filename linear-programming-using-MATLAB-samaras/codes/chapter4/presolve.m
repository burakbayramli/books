function [A, c, b, Eqin, c0, infeasible, unbounded] = ...
	presolve(A, c, b, Eqin, c0)
% Filename: presolve.m
% Description: the function is an implementation of the 
% main file that calls all the presolve techniques
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [A, c, b, Eqin, c0, infeasible, unbounded] = ...
%	presolve(A, c, b, Eqin, c0)
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
% -- c0: constant term of the objective function
%
% Output:
% -- A: presolved matrix of coefficients of the constraints 
%    (size m x n)
% -- c: presolved vector of coefficients of the objective 
%    function (size n x 1)
% -- b: presolved vector of the right-hand side of the 
%    constraints (size m x 1)
% -- Eqin: presolved vector of the type of the constraints 
%    (size m x 1)
% -- c0: updated constant term of the objective function
% -- infeasible: flag variable showing if the LP is 
%    infeasible or not
% -- unbounded: flag variable showing if the LP is unbounded 
%    or not
  
[m, n] = size(A); % find size of A
% store the initial size of A to calculate the difference 
% after the presolve analysis
minit = m;
ninit = n;
infeasible = 0;
unbounded = 0;
% if the constant term does not exist, set it to zero
if(~exist('c0'))
	c0 = 0;
end
mprevious = 0;
nprevious = 0;
% call presolve methods until no change in matrix A
while(mprevious ~= m || nprevious ~= n)
	% call presolve technique to eliminate zero rows
	[A, b, Eqin, infeasible] = eliminateZeroRows(A, b, Eqin);
	if(infeasible == 1)
		return;
	end
	% call presolve technique to eliminate zero columns
	[A, c, unbounded] = eliminateZeroColumns(A, c);
	if(unbounded == 1)
		return;
	end
	% call presolve technique to eliminate kton equality
	% constraints (as an example, we set kton = 2, i.e.,
    % we eliminate doubleton and singleton constraints)
	[A, c, b, Eqin, c0, infeasible] = ...
		eliminateKtonEqualityConstraints(A, c, b, ...
            Eqin, c0, 2);
	if(infeasible == 1)
		return;
	end
	% call presolve technique to eliminate singleton 
	% inequality constraints
	[A, c, b, Eqin, infeasible] = ...
		eliminateSingletonInequalityConstraints(A, c, ...
            b, Eqin);
	if(infeasible == 1)
		return;
	end
	% call presolve technique to eliminate dual singleton 
	% equality constraints
	[A, c, b, Eqin, infeasible] = ...
		eliminateDualSingletonInequalityConstraints(A, ...
            c, b, Eqin);
	if(infeasible == 1)
		return;
	end
	% call presolve technique to eliminate implied free 
	% singleton columns
	[A, c, b, Eqin, c0] = ...
		eliminateImpliedFreeSingletonColumns(A, c, b, ...
        Eqin, c0);
	% call presolve technique to eliminate redundant 
	% columns
	[A, c] = eliminateRedundantColumns(A, c, b, Eqin);
	% call presolve technique to eliminate implied bounds 
	% on rows
	[A, b, Eqin] = eliminateImpliedBoundsonRows(A, ...
        b, Eqin);
	% call presolve technique to eliminate zero columns
	[A, c, unbounded] = eliminateZeroColumns(A, c);
	if(unbounded == 1)
		return;
	end
	mprevious = m;
	nprevious = n;
	[m, n] = size(A);
end
% add slack variables
Atemp = A;
id = find(Eqin ~= 0);
[m, ~] = size(A);
for i = 1:length(id)
	if(Eqin(id(i)) == -1) % <= constraints
		ei = zeros(m, 1);
		ei(id(i)) = 1;
		Atemp = [Atemp ei];
	else % >= constraints
		ei = zeros(m, 1);
		ei(id(i)) = -1;
		Atemp = [Atemp ei];
	end
end
% call presolve technique to make the coefficient matrix 
% structurally full rank 
[A, b, Eqin] = fullRank(A, Atemp, b, Eqin);
% call presolve technique to eliminate redundant rows
[A, b, Eqin, infeasible] = eliminateRedundantRows(A,...
    b, Eqin);
if(infeasible == 1)
	return;
end
% output statistics after the presolve analysis
[m, n] = size(A);
fprintf('========================================\n')
fprintf(' (%i Constraints eliminated) \n', minit - m);
fprintf(' (%i Variables eliminated) \n', ninit - n);
fprintf(' ======================================== \n')
fprintf(' ========FINAL REDUCTION (RESULTS)======= \n')
fprintf(' ======================================== \n')
fprintf(' %i Constraints \n', m);
fprintf(' %i Variables \n', n);
fprintf(' %i NonZeros \n', nnz(A));
fprintf(' ======================================== \n')
end