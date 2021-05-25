%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     In this test case heat diffusion in solid flange is simulated
%--------------------------------------------------------------------------

% Initialize
cfdStartSession;

% Read OpenFOAM Files
cfdReadOpenFoamFiles;

% Define heat diffusion equation
cfdSetupEquation('T');

cfdSetEnergyTerms({'Diffusion'});

if cfdIsTransient
    cfdAddTerm('T', 'Transient');
else
    cfdAddTerm('T', 'FalseTransient');
end

% Run case
cfdRunCase;