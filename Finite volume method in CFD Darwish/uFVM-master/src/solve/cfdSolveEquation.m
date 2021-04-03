function cfdSolveEquation(theEquationName, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Solve algebraic linear system
%--------------------------------------------------------------------------

if nargin==1
    iComponent = 1;
end

% Get General linear solver settings
foamDict = cfdGetFoamDict;
solver = foamDict.fvSolution.solvers.(theEquationName).solver;
maxIter = foamDict.fvSolution.solvers.(theEquationName).maxIter;
tolerance = foamDict.fvSolution.solvers.(theEquationName).tolerance;
relTol = foamDict.fvSolution.solvers.(theEquationName).relTol;

if strcmp(solver, 'GAMG')   
    % Get GAMG settings
    preconditioner = foamDict.fvSolution.solvers.(theEquationName).preconditioner;
    nPreSweeps = foamDict.fvSolution.solvers.(theEquationName).nPreSweeps;
    nPostSweeps = foamDict.fvSolution.solvers.(theEquationName).nPostSweeps;
    nFinestSweeps = foamDict.fvSolution.solvers.(theEquationName).nFinestSweeps;
    
    [initRes, finalRes] = cfdApplyAMG(preconditioner,maxIter,tolerance,relTol,nPreSweeps,nPostSweeps,nFinestSweeps);
elseif strcmp(solver, 'smoothSolver')
    smoother = foamDict.fvSolution.solvers.(theEquationName).smoother;
    [initRes, finalRes] = cfdSolveAlgebraicSystem(1,smoother,maxIter,tolerance,relTol);
else
    error('%s not defined', solver);
end

% Store linear solver residuals
theEquation = cfdGetModel(theEquationName);
theEquation.residuals.initResidual(iComponent) = initRes;
theEquation.residuals.finalResidual(iComponent) = finalRes;
cfdSetModel(theEquation);