function cfdAssembleAndCorrectScalarEquation(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles and corrects scalar equation
%--------------------------------------------------------------------------

theCoefficients = cfdSetupCoefficients;
cfdSetCoefficients(theCoefficients);

% Assemble Equation
cfdAssembleEquation(theEquationName);

% Solve and correct scalar equation
cfdSolveScalarEquation(theEquationName);

% Reset coefficients to zero
cfdZeroCoefficients;

