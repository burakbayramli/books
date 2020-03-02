function [xsol, fval, exitflag, iterations] = ...
    ipdipm(A, c, b, Eqin, MinMaxLP, c0, ...
    maxIterations, tol, etaMin, scalingTechnique)
% Filename: ipdipm.m
% Description: the function is an implementation of 
% Mehrotra's Predictor-Corrector infeasible 
% primal-dual interior point method
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [xsol, fval, exitflag, iterations] = ...
%   ipdipm(A, c, b, Eqin, MinMaxLP, c0, ...
%   maxIterations, tol, etaMin, scalingTechnique)
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
% -- c0: constant term of the objective function
%    (optional: default value 0)
% -- maxIterations: maximum number of iterations to
%    perform if an optimal solution is not found
%    (optional: default value 100)
% -- tole: tolerance for the termination criterion 
%    (optional: default value 1e-08)
% -- etaMin: parameter eta (optional: default 
%    value 0.995)
% -- scalingTechnique: the scaling method to be used
%    (0: no scaling, 1: arithmetic mean, 2: de Buchet for 
%    the case p = 1, 3: de Buchet for the case p = 2, 
%    4: de Buchet for the case p = Inf, 5: entropy, 
%    6: equilibration, 7: geometric mean, 8: IBM MPSX, 
%    9: LP-norm for the case p = 1, 10: LP-norm for 
%    the case p = 2, 11: LP-norm for the case p = Inf) 
%    (optional: default value 6)
%
% Output:
% -- xsol: the solution found (size n x 1)
% -- fval: the value of the objective function at the 
%    solution x
% -- exitflag: the reason that the algorithm terminated 
%    (0: optimal solution found, 1: the LP problem is
%    infeasible, -1: the input data is not logically 
%    or numerically correct)
% -- iterations: the number of iterations

% initialize output variables
xsol = [];
fval = 0;
exitflag = 0;
iterations = 0;
% set default values to missing inputs
if ~exist('maxIterations')
	maxIterations = 100;
end
if ~exist('tol')
	tol = 1e-8;
end
if ~exist('etaMin')
	etaMin = 0.995;
end
if ~exist('scalingTechnique')
	scalingTechnique = 6;
end
[m, n] = size(A); % find the size of matrix A
[m2, n2] = size(c); % find the size of vector c
[m3, n3] = size(Eqin); % find the size of vector Eqin
[m4, n4] = size(b); % find the size of vector b
% check if input data is logically correct
if n2 ~= 1
	disp('Vector c is not a column vector.')
	exitflag = -1;
	return
end
if n ~= m2
	disp(['The number of columns in matrix A and ' ...
        'the number of rows in vector c do not match.'])
	exitflag = -1;
	return
end
if m4 ~= m
	disp(['The number of the right-hand side values ' ...
        'is not equal to the number of constraints.'])
	exitflag = -1;
	return
end
if n3 ~= 1
	disp('Vector Eqin is not a column vector')
	exitflag = -1;
	return
end
if n4 ~= 1
	disp('Vector b is not a column vector')
	exitflag = -1;
	return
end
if m4 ~= m3
	disp('The size of vectors Eqin and b does not match')
	exitflag = -1;
	return
end
% if the type of optimization is maximization, then multiply 
% vector c and constant c0 by -1
if MinMaxLP == 1
	c = -c;
	c0 = -c0;
end
% perform the presolve analysis
disp('---- P R E S O L V E    A N A L Y S I S ----')
[A, c, b, Eqin, c0, infeasible, unbounded] = ...
 	presolve(A, c, b, Eqin, c0);
if infeasible == 1 % the LP problem is infeasible
    disp('The LP problem is infeasible')
	exitflag = 1;
 	return
end
if unbounded == 1 % the LP problem is unbounded
    disp('The LP problem is unbounded')
	exitflag = 2;
	return
end
[m, n] = size(A); % find the new size of matrix A
[m2, ~] = size(c); % find the new size of vector c
[m3, ~] = size(Eqin); % find the size of vector Eqin
% scale the LP problem using the selected scaling
% technique
disp('---- S C A L I N G ----')
if scalingTechnique == 1 % arithmetic mean
 	[A, c, b, ~, ~] = arithmeticMean(A, c, b);
% de buchet for the case p = 1
elseif scalingTechnique == 2
 	[A, c, b, ~, ~] = debuchet1(A, c, b);
% de buchet for the case p = 2
elseif scalingTechnique == 3
 	[A, c, b, ~, ~] = debuchet2(A, c, b);
% de buchet for the case p = Inf
elseif scalingTechnique == 4
 	[A, c, b, ~, ~] = debuchetinf(A, c, b);
elseif scalingTechnique == 5 % entropy
 	[A, c, b, ~, ~] = entropy(A, c, b);
elseif scalingTechnique == 6 % equilibration
 	[A, c, b, ~, ~] = equilibration(A, c, b);
elseif scalingTechnique == 7 % geometric mean
 	[A, c, b, ~, ~] = geometricMean(A, c, b);
elseif scalingTechnique == 8 % IBM MPSX
 	[A, c, b, ~, ~, ~, ~] = ibmmpsx(A, c, b);
% LP-norm for the case p = 1
elseif scalingTechnique == 9
 	[A, c, b, ~, ~] = lpnorm1(A, c, b);
% LP-norm for the case p = 2
elseif scalingTechnique == 10
 	[A, c, b, ~, ~] = lpnorm2(A, c, b);
% LP-norm for the case p = Inf
elseif scalingTechnique == 11
 	[A, c, b, ~, ~] = lpnorminf(A, c, b);
end
flag = isequal(Eqin, zeros(m3, 1));
if flag ~= 1
	% some or all constraints are inequalities
	% add slack variables
	axm = nnz(Eqin);
	c(m2 + 1:m2 + axm, :) = 0;
	A(:, n + 1:n + axm) = zeros(m, axm);
	curcol = 1;
	for i = 1:m3
		% 'greater than or equal to' inequality constraint
		if Eqin(i, 1) == 1
			A(i, n + curcol) = -1;
			curcol = curcol + 1;
		% 'less than or equal to' inequality constraint
		elseif Eqin(i, 1) == -1
			A(i, n + curcol) = 1;
			curcol = curcol + 1;
		% unrecognized type of constraint
		elseif Eqin(i,1) ~= 0
			disp('Vector Eqin is not numerically correct.')
			exitflag = -1;
			return
		end
	end
end
[m, n] = size(A);  % new size of matrix A
% calculate the density of matrix A
density = (nnz(A) / (m * n)) * 100;
% if the density of matrix A is less than 20%, then 
% use sparse algebra for faster computations
if density < 20
	A = sparse(A);
	c = sparse(c);
	b = sparse(b);
end
% tolerance for detecting infeasible LPs
infTole = 1.e15 * max([normest(A), normest(b), ...
    normest(c)]);
% calculate the starting point using Mehrotra's 
% heuristic
x = A' * ((A * A') \ b);
w = (A * A') \ (A * c);
s = c - A' * w;
delta_x = max(-1.5 * min(x), 0);
delta_s = max(-1.5 * min(s), 0);
e = ones(n, 1);
delta_x_s = 0.5 * (x + delta_x * e)' * ...
    (s + delta_s * e);
delta_x_c = delta_x + delta_x_s / ...
    (sum(s) + n * delta_s);
delta_s_c = delta_s + delta_x_s / ...
    (sum(x) + n * delta_x);
x = x + delta_x_c * e;
s = s + delta_s_c * e;
% iterate until maxIterations or the LP problem is
% optimal or infeasible
for i = 1:maxIterations
	iterations = iterations + 1;
	% calculate the residuals and the duality measure
	rd = A' * w + s - c; % dual residual
	rp = A * x - b; % primal residual
	rc = x .* s; % complementarity residual
	mu = x' * s / n; % duality measure
	residual = normest(rc, 1) / (1 + abs(b' * w));
	% heuristic to detect infeasible LPs
	if isnan(residual) || normest(x) + normest(s) ...
			>= infTole
		disp('The LP problem is infeasible!');
		exitflag = 1;
		break;
	end
	% the termination criterion
	if max(mu, max(normest(rp), normest(rd))) <= tol
		xsol = x;
		fval = c' * x; % calculate the objective function value
		if MinMaxLP == 1 % maximization
			fval = full(-(fval + c0));
		else % minimization
			fval = full(fval + c0);
		end
		exitflag = 0;
		disp('The LP problem is optimal')
		break;  
	end
	% formulate the coefficient matrix of the 
	% linear systems
	M = A * diag(x ./ s) * A';
	[R, p] = chol(M); % factorize M
	if p > 0 % if matrix M is positive definite, we
		% assume that the LP problem is optimal
		xsol = x;
		fval = c' * x; % calculate the objective function value
		if MinMaxLP == 1 % maximization
			fval = full(-(fval + c0));
		else % minimization
			fval = full(fval + c0);
		end
		exitflag = 0;
		disp('The LP problem is optimal')
		break; 
	end
	% predictor step
	rhs = rp - A * ((rc - x .* rd) ./ s);
	dw = R \ (R' \ rhs);
	ds = rd - A' * dw; 
	dx = (rc - x .* ds) ./ s;
	alpha_p = 1 / max([1; dx ./ x]); 
	alpha_d = 1 / max([1; ds ./ s]);
	% centering parameter step
	mun = ((x - alpha_p * dx)' * (s - alpha_d * ds)) / n;
	sigma = (mun / mu) ^ 3;
	% corrector step
	rc = rc - sigma * mu + dx .* ds;
	rhs = rp - A * ((rc - x .* rd) ./ s);
	dw = R \ (R' \ rhs);
	ds = rd - A' * dw;
	dx = (rc - x .* ds) ./ s;
	eta = max(etaMin, 1 - mu);
	alpha_p = eta / max([eta; dx ./ x]); 
	alpha_d = eta / max([eta; ds ./ s]); 
	% update step
	x = x - alpha_p * dx; 
	w = w - alpha_d * dw; 
	s = s - alpha_d * ds;
	% print intermediate results every 5 iterations
	if mod(iterations, 5) == 0
		% calculate the value of the objective 
		% function
		if MinMaxLP == 1 % maximization
			fval = full(-(c' * x + c0));
		else % minimization
			fval = full(c' *x + c0);
		end
		fprintf(['Iteration %i - objective value: ' ... 
			'%f\n'], iterations, fval);
	end
end
end