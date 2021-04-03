%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     In this test case an air flow past a NACA0012 airfoil is simulated
%--------------------------------------------------------------------------

% Initialize
cfdStartSession;

% Read OpenFOAM Files
cfdReadOpenFoamFiles;

% Define equations to be solved
cfdDefineMomentumEquation;
cfdDefineContinuityEquation;

% Run case
cfdRunCase;