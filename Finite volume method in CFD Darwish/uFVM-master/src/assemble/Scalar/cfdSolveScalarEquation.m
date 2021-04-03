function cfdSolveScalarEquation(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

% Solve Equation
cfdSolveEquation(theEquationName);

% Correct Equation
cfdCorrectScalarEquation(theEquationName);

% Updates
cfdUpdateGradient(theEquationName);
cfdUpdateScale(theEquationName);
