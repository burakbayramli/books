function finalResidual = cfdApplyWCycle(gridLevel,preconditioner,maxLevels,nPreSweeps,nPostSweeps,relTol,nFinestSweeps)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Apply W-Cycle
%--------------------------------------------------------------------------

while gridLevel<maxLevels
    cfdRestrict(gridLevel,preconditioner,nPreSweeps);
    gridLevel = gridLevel + 1;
end
%
% Smooth coarsest level
%
cfdSolveAlgebraicSystem(gridLevel,preconditioner,nPostSweeps);
%
%
cfdProlongate(gridLevel,preconditioner,nPostSweeps,relTol);
%
cfdRestrict(gridLevel-1,preconditioner);
%
% Smooth coarsest level
%
cfdSolveAlgebraicSystem(gridLevel,preconditioner,nPostSweeps);
%
%
cfdProlongate(gridLevel,preconditioner,nPostSweeps,relTol);
%
cfdProlongate(gridLevel-1,preconditioner,nPostSweeps,relTol);
%
cfdRestrict(gridLevel-2,preconditioner);
%
cfdRestrict(gridLevel-1,preconditioner,nPreSweeps);
%
% Smooth coarsest level
%
cfdSolveAlgebraicSystem(gridLevel,preconditioner,nPostSweeps);
%
%
cfdProlongate(gridLevel,preconditioner,nPostSweeps,relTol);
%
cfdProlongate(gridLevel-1,preconditioner,nPostSweeps,relTol);
%
cfdProlongate(gridLevel-2,preconditioner,nPostSweeps,relTol);
%
cfdRestrict(gridLevel-3,preconditioner);
%
cfdRestrict(gridLevel-2,preconditioner,nPreSweeps);
%
cfdRestrict(gridLevel-1,preconditioner,nPreSweeps);
%
% Smooth coarsest level
%
cfdSolveAlgebraicSystem(gridLevel,preconditioner,nPostSweeps);
%
%
cfdProlongate(gridLevel,preconditioner,nPostSweeps,relTol);
%
cfdProlongate(gridLevel-1,preconditioner,nPostSweeps,relTol);
%
cfdRestrict(gridLevel-2,preconditioner);
%
cfdRestrict(gridLevel-1,preconditioner,nPreSweeps);
%
% Smooth coarsest level
%
cfdSolveAlgebraicSystem(gridLevel,preconditioner,nPostSweeps);
%
%
cfdProlongate(gridLevel,preconditioner,nPostSweeps,relTol);
%
cfdRestrict(gridLevel-1,preconditioner);
%
% Smooth coarsest level
%
cfdSolveAlgebraicSystem(gridLevel,preconditioner,nPostSweeps);
%
%
while gridLevel>1
    if gridLevel==1
        cfdProlongate(gridLevel,preconditioner,nFinestSweeps,relTol);
    else
        cfdProlongate(gridLevel,preconditioner,nPostSweeps,relTol);
    end
    gridLevel = gridLevel - 1;
end
%
% Calculate final residual
%
theCoefficients = cfdGetCoefficients;
residualsArray = cfdComputeResidualsArray(theCoefficients);
finalResidual = sum(abs(residualsArray));