function cfdRunFalseTransientCase
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
cfdInitDirectories;

% Pre-updates (necessary on startup)
cfdUpdateFieldsForAllBoundaryPatches;
cfdUpdateGradients;
cfdUpdateScales;

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
    
    % Navier-Stokes and pressure correction
    cfdAssembleAndCorrectNSSystem;
    
    % Energy
    cfdAssembleAndCorrectEnergyEquation;
    
    % Post actions
    cfdPlotRes;
    cfdPostResults(totalNumberOfIterations);    
	cfdWriteResults(totalNumberOfIterations);        
end