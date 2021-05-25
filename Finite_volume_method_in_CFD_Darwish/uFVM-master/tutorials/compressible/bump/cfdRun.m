%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     In this test case a compressible air flow over a bump is simulated
%--------------------------------------------------------------------------

% Initialize
cfdStartSession;

% Read OpenFOAM Files
cfdReadOpenFoamFiles;

% Define equations to be solved
cfdDefineMomentumEquation;
cfdDefineContinuityEquation;
cfdDefineEnergyEquation;

% Run case
cfdRunCase;