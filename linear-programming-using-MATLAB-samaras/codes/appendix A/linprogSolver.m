function [xsol, fval, exitflag, iterations] = ...
    linprogSolver(A, c, b, Eqin, MinMaxLP, c0, ...
    algorithm)
% Filename: linprogSolver.m
% Description: the function is a MATLAB code to solve LPs
% using the linprog solver
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [x, fval, exitflag, iterations] = ...
%   linprogSolver(A, c, b, Eqin, MinMaxLP, c0, ...
%   algorithm)
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
% -- algorithm: the LP algorithm that will be used 
%    (possible values: 'interior-point', 'dual-simplex', 
%    'simplex', 'active-set')
%
% Output:
% -- xsol: the solution found by the solver (size m x 1)
% -- fval: the value of the objective function at the 
%    solution xsol
% -- exitflag: the reason that the algorithm terminated 
%    (1: the solver converged to a solution x, 0: the 
%    number of iterations exceeded the MaxIter option, 
%    -1: the input data is not logically or numerically 
%    correct, -2: no feasible point was found, -3: the LP 
%    problem is unbounded, -4: NaN value was encountered 
%    during the execution of the algorithm, -5: both primal 
%    and dual problems are infeasible, -7: the search 
%    direction became too small and no further progress 
%    could be made)
% -- iterations: the number of iterations

% set default values to missing inputs
if ~exist('MinMaxLP')
	MinMaxLP = -1;
end
if ~exist('c0')
	c0 = 0;
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
% if the problem is a maximization problem, transform it to
% a minimization problem
if MinMaxLP == 1 
    c = -c;
end
Aeq = []; % matrix constraint of equality constraints
beq = []; % right-hand side of equality constraints
% check if all constraints are equalities
flag = isequal(zeros(m3, 1), Eqin);
if flag == 0 % some or all constraints are inequalities
	indices = []; % indices of the equality constraints
	for i = 1:m3
        if Eqin(i, 1) == 0 % find the equality constraints
            indices = [indices i];
        % convert 'greater than or equal to' type of 
        % constraints to 'less than or equal to' type
        % of constraints
        elseif Eqin(i, 1) == 1
            A(i, :) = -A(i, :);
            b(i) = -b(i);
        end
	end
	% create the matrix constraint of equality constraints
	Aeq = A(indices, :);
	A(indices, :) = []; % delete equality constraints
	% create the right-hand side of equality constraints
	beq = b(indices);
	b(indices) = []; % delete equality constraints
else % all constraints are equalities
    Aeq = A;
    beq = b;
    A = [];
    b = [];
end
lb = zeros(n, 1); % create zero lower bounds
% choose LP algorithm
options = optimoptions(@linprog, 'Algorithm', algorithm);
% call the linprog solver
[xsol, fval, exitflag, output] = linprog(c, A, b, ...
    Aeq, beq, lb, [], [], options);
iterations = output.iterations;
% calculate the value of the objective function
if MinMaxLP == 1 % maximization
	fval = -(fval + c0);
else % minimization
	fval = fval + c0;
end
end