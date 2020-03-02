function [xsol, fval, exitflag, iterations] = ...
    cplexOptimizer(mpsFullPath, algorithm, ...
    simplexTol, barrierTol, maxTime, simplexMaxIter, ...
    barrierMaxIter, parallelMode, presolve, ...
    numPresolvePasses, factorFreq)
% Filename: cplexOptimizer.m
% Description: the function is a MATLAB code to solve LPs
% using CPLEX
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [xsol, fval, exitflag, iterations] = ...
%     cplexOptimizer(mpsFullPath, algorithm, ...
%     simplexTol, barrierTol, maxTime, simplexMaxIter, ...
%     barrierMaxIter, parallelMode, presolve, ...
%     numPresolvePasses, factorFreq)
% 
% Input:
% -- mpsFullPath: the full path of the MPS file
% -- algorithm: the CPLEX algorithm that will be used 
%    (optional, possible values: 0 - automatic, 1 - 
%    primal simplex, 2 - dual simplex, 3 - network 
%    simplex, 4 - barrier, 5 - sifting, 6 - concurrent 
%    optimizers)
% -- simplexTol: the tolerance for simplex type
%    algorithms (optional)
% -- barrierTol: the tolerance for the barrier 
%    algorithm (optional)
% -- maxTime: the maximum execution time in seconds
% -- (optional)
% -- simplexMaxIter: the maximum number of iterations
% -- for simplex type algorithms (optional)
% -- barrierMaxIter: the maximum number of iterations
% -- for the barrier algorithm (optional)
% -- parallelModel: the parallel model (optional, 
%    possible values: -1: opportunistic, 0: automatic, 
%    1: deterministic)
% -- presolve: decides whether CPLEX applies presolve 
%    (optional, possible values: 0: no, 1: yes)
% -- numPresolvePasses: the number of presolver passes
%    (optional)
% -- factorFreq: every factorFreq number of iterations, 
%    the basis inverse is re-computed from scratch 
%    (optional)
%
% Output:
% -- xsol: the solution found by the solver (size m x 1)
% -- fval: the value of the objective function at the 
%    solution xsol
% -- exitflag: the reason that the algorithm terminated 
%    (1: the solver converged to a solution x, 2: the
%    LP problem is unbounded, 3: the LP problem is 
%    infeasible, 10: the number of iterations exceeded 
%    the maxIter option, 11: time reached)
% -- iterations: the number of iterations

% set user defined values to options
cplex = Cplex;
if exist('algorithm')
	cplex.Param.lpmethod.Cur = algorithm;
end
if exist('simplexTol')
	cplex.Param.simplex.tolerances.optimality.Cur ...
        = simplexTol;
end
if exist('barrierTol')
	cplex.Param.barrier.convergetol.Cur = barrierTol;
end
if exist('maxTime')
	cplex.Param.timelimit.Cur = maxTime;
end
if exist('simplexMaxIter')
	cplex.Param.simplex.limits.iterations.Cur = ...
        simplexMaxIter;
end
if exist('barrierMaxIter')
	cplex.Param.barrier.limits.iteration.Cur = ...
        barrierMaxIter;
end
if exist('parallelMode')
	cplex.Param.parallel.Cur = parallelMode;
end
if exist('presolve')
	cplex.Param.preprocessing.presolve.Cur = ...
        presolve;
end
if exist('numPresolvePasses')
	cplex.Param.preprocessing.numpass.Cur = ...
        numPresolvePasses;
end
if exist('factorFreq')
	cplex.Param.simplex.refactor.Cur = factorFreq;
end
cplex.DisplayFunc = [];
% read the MPS file
cplex.readModel(mpsFullPath);
% call CPLEX solver
cplex.solve;
% export solution
xsol = cplex.Solution.x;
fval = cplex.Solution.objval;
exitflag = cplex.Solution.status;
if isfield(cplex.Solution, 'itcnt')
	iterations = cplex.Solution.itcnt;
else
	iterations = cplex.Solution.baritcnt;
end
end