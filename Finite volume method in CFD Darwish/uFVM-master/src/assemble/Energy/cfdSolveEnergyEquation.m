function cfdSolveEnergyEquation
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

% Solve Equation
cfdSolveEquation('T');

% Correct Equation
cfdCorrectEnergyEquation;
