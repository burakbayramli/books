function finalResidual = cfdApplyVCycle(gridLevel,preconditioner,maxLevels,nPreSweeps,nPostSweeps,relTol,nFinestSweeps)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Apply V-Cycle
%--------------------------------------------------------------------------

% Restriction phase
while gridLevel<maxLevels
    % pre-sweep
    cfdSolveAlgebraicSystem(gridLevel,preconditioner,nPreSweeps);
    
    % Restrict residuals
    cfdRestrict(gridLevel);
    
    % Update level
    gridLevel = gridLevel + 1;
end

% Smoothening the coarsest level
cfdSolveAlgebraicSystem(gridLevel,preconditioner,nPostSweeps);

% Prolongation phase
while gridLevel>1
    if gridLevel==2
        % Prolongate correction to finer solution
        cfdProlongate(gridLevel);
        
        % post-sweep finest level
        cfdSolveAlgebraicSystem(gridLevel-1,preconditioner,nFinestSweeps); 
    else        
        % Prolongate correction to finer solution
        cfdProlongate(gridLevel);
        
        % post-sweep
        cfdSolveAlgebraicSystem(gridLevel-1,preconditioner,nPostSweeps);        
    end    
    gridLevel = gridLevel - 1;
end

% Calculate final residual
theCoefficients = cfdGetCoefficients;
residualsArray = cfdComputeResidualsArray(theCoefficients);
finalResidual = sum(abs(residualsArray));