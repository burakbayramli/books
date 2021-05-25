function cfdAssembleContinuityEquation
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles continuity equation
%--------------------------------------------------------------------------

% Pre-assemble
cfdPreAssembleContinuityEquation;

% Assemble Terms
cfdAssembleContinuityEquationTerms;

% Post Assemble Equation (under-relaxation, etc)
cfdPostAssembleContinuityEquation;