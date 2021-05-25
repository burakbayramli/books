function cfdAssembleEquation(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles equation
%--------------------------------------------------------------------------

% Pre-assemble
cfdPreAssembleEquation(theEquationName);

% Assemble Terms
cfdAssembleEquationTerms(theEquationName);

% Post Assemble Equation (under-relaxation, etc)
cfdPostAssembleScalarEquation(theEquationName);