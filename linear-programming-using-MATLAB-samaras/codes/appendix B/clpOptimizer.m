function [xsol, fval, exitflag, iterations] = ...
    clpOptimizer(mpsFullPath, algorithm, primalTol, ...
    dualTol, maxIter, maxTime, displayLevel, objBias, ...
    numPresolvePasses, factorFreq, numberRefinements, ...
    primalObjLim, dualObjLim, numThreads, abcState)
% Filename: clpOptimizer.m
% Description: the function is a MATLAB code to solve LPs
% using CLP (via OPTI Toolbox)
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [xsol, fval, exitflag, iterations] = ...
%    clpOptimizer(mpsFullPath, algorithm, primalTol, ...
%    dualTol, maxIter, maxTime, display, objBias, ...
%    numPresolvePasses, factorFreq, numberRefinements, ...
%    primalObjLim, dualObjLim, numThreads, abcState)
% 
% Input:
% -- mpsFullPath: the full path of the MPS file
% -- algorithm: the CLP algorithm that will be used 
%    (optional, possible values: 'DualSimplex', 
%    'PrimalSimplex', 'PrimalSimplexOrSprint', 
%    'Barrier', 'BarrierNoCross', 'Automatic')
% -- primalTol: the primal tolerance (optional)
% -- dualTol: the dual tolerance (optional)
% -- maxIter: the maximum number of iterations
% -- (optional)
% -- maxTime: the maximum execution time in seconds
% -- (optional)
% -- displayLevel: the display level (optional,  
%    possible values: 0, 1 -> 100 increasing)
% -- objBias: the objective bias term (optional)
% -- numPresolvePasses: the number of presolver passes
%    (optional)
% -- factorFreq: every factorFreq number of iterations, 
%    the basis inverse is re-computed from scratch 
%    (optional)
% -- numberRefinements: the number of iterative simplex 
%    refinements (optional)
% -- primalObjLim: the primal objective limit (optional)
% -- dualObjLim: the dual objective limit (optional)
% -- numThreads: the number of Cilk worker threads 
%    (optional, only with Aboca CLP build)
% -- abcState: Aboca's partition size (optional)
%
% Output:
% -- xsol: the solution found by the solver (size m x 1)
% -- fval: the value of the objective function at the 
%    solution xsol
% -- exitflag: the reason that the algorithm terminated 
%    (1: the solver converged to a solution x, 0: the 
%    number of iterations exceeded the maxIter option or
%    time reached, -1: the LP problem is infeasible or
%    unbounded)
% -- iterations: the number of iterations

% set user defined values to options
opts = optiset('solver', 'clp');
if exist('algorithm')
	opts.solverOpts.algorithm = algorithm;
end
if exist('primalTol')
	opts.solverOpts.primalTol = primalTol;
	opts.tolrfun = primalTol;
end
if exist('dualTol')
	opts.solverOpts.dualTol = dualTol;
end
if exist('maxIter')
	opts.maxiter = maxIter;
else
    opts.maxiter = 1000000;
end
if exist('maxTime')
	opts.maxtime = maxTime;
else
	opts.maxtime = 1000000;
end
if exist('displayLevel')
	opts.display = displayLevel;
end
if exist('objBias')
	opts.solverOpts.objbias = objBias;
end
if exist('numPresolvePasses')
	opts.solverOpts.numPresolvePasses = numPresolvePasses;
end
if exist('factorFreq')
	opts.solverOpts.factorFreq = factorFreq;
end
if exist('numberRefinements')
	opts.solverOpts.numberRefinements = numberRefinements;
end
if exist('primalObjLim')
	opts.solverOpts.primalObjLim = primalObjLim;
end
if exist('dualObjLim')
	opts.solverOpts.dualObjLim = dualObjLim;
end
if exist('numThreads')
	opts.solverOpts.numThreads = numThreads;
end
if exist('abcState')
	opts.solverOpts.abcState = abcState;
end
% read the MPS file
prob = coinRead(mpsFullPath);
% call CLP solver
[xsol, fval, exitflag, info] = opti_clp([], prob.f, ...
    prob.A, prob.rl, prob.ru, prob.lb, prob.ub, opts);
iterations = info.Iterations;
end