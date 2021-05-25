%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     In this test case the square cavity problem is considered with a
%     uniform velocity profile throughout the domain. The objective is to
%     investigate the convection schemes (the default now is set to first
%     order upwind). The equation to be solved is given by the following
%     pde: d(rho*phi)/dt + div(rho*U*phi) = 0
%--------------------------------------------------------------------------

% Initialize
cfdStartSession;

% Read OpenFOAM Files
cfdReadOpenFoamFiles;

% Define transient-convection equation
cfdSetupEquation('phi');
cfdSetTerms('phi', {'Transient', 'Convection'});

% Define mdot_f field
cfdSetupMeshField('mdot_f', 'surfaceScalarField', 'dimensions', [0,0,0,0,0,0,0]);  
cfdInitializeMdotFromU;

% Run case
cfdPrintHeader;

% Setup algebraic coefficients
theCoefficients = cfdSetupCoefficients;
cfdSetCoefficients(theCoefficients);

% Setup assembly fluxes
cfdSetupFluxes;

% Initialize runtime
cfdInitTime;
cfdInitDirectories('phi');

% Pre-updates (necessary on startup)
cfdUpdateScalarFieldForAllBoundaryPatches('phi');
cfdUpdateGradient('phi');
cfdUpdateScale('phi');

% Start transient loop
totalNumberOfIterations = 0;
while (cfdDoTransientLoop)
    
    % Time settings
    cfdUpdateRunTime;
    
    % Copy current time fields to previous time fields
    thePhiField = cfdGetMeshField('phi');
    thePhiField.prevTimeStep.phi = thePhiField.phi;  
    cfdSetMeshField(thePhiField);
    
    % Print current simulation time
    cfdPrintCurrentTime;
    
    % Start loop at current time step
    for iter=1:10
        % Update number of global iters
        totalNumberOfIterations = totalNumberOfIterations + 1;
        
        % Print
        cfdPrintIteration(totalNumberOfIterations);
        cfdPrintResidualsHeader;
        
        % Store previous iter field
        thePhiField = cfdGetMeshField('phi');
        thePhiField.prevIter.phi = thePhiField.phi;  
        cfdSetMeshField(thePhiField);
        
        % Scalar equation
        cfdAssembleAndCorrectScalarEquation('phi');
        
        % Post actions
        cfdPlotEquationRes('phi');
        cfdPostEquationResults('phi',totalNumberOfIterations);
        cfdWriteResults(totalNumberOfIterations);
    end
end