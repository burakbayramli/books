function cfdAssembleMomentumEquation(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles momentum equation
%--------------------------------------------------------------------------

% Pre-assemble
cfdPreAssembleMomentumEquation(iComponent);

% Assemble Terms
cfdAssembleMomentumEquationTerms(iComponent);

% Post Assemble Equation (under-relaxation, etc)
cfdPostAssembleMomentumEquation(iComponent);

