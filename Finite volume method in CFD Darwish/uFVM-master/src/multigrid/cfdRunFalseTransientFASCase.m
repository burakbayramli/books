function cfdRunFalseTransientFASCase
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function cfdRuns steady state case
%--------------------------------------------------------------------------

cfdPrintHeader;

% Setup algebraic coefficients
theCoefficients = cfdSetupCoefficients;
cfdSetCoefficients(theCoefficients);

% Setup assembly fluxes
cfdSetupFluxes;

% Initialize cfdRun time
cfdInitTime;
cfdUpdateFieldsForAllBoundaryPatches;
cfdUpdateGradients;
cfdUpdateScales;

% Create multigrid meshes
cfdAgglomerateElements(3);

% Start steady false transience loop
totalNumberOfIterations = 0;
while (cfdDoFalseTransientLoop)
    
    % Update number of iters
    totalNumberOfIterations = totalNumberOfIterations + 1;
    
    % Time settings
    cfdUpdateRunTime;
    
    % Print
    cfdPrintIteration(totalNumberOfIterations);
    cfdPrintResidualsHeader;
    
    % Updates
    cfdUpdatePrevIter;
    cfdUpdateProperties;
    
    % Momentum
    cfdAssembleAndCorrectMomentumEquation;
    
    % Continuity
    cfdAssembleAndCorrectContinuityEquation;
    
    % Post actions
    cfdPlotRes;
    cfdPostResults;
    cfdWriteResults(totalNumberOfIterations);
end