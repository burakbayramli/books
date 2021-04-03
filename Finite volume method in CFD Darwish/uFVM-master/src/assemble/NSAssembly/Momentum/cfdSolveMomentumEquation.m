function cfdSolveMomentumEquation(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function solves momentum equation
%--------------------------------------------------------------------------

% Solve Equation
cfdSolveEquation('U', iComponent);

% Correct Equation
cfdCorrectVelocityEquation(iComponent);

% Updates
cfdUpdateGradient('U');
cfdUpdateScale('U');