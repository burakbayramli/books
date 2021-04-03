function isAvailable = cfdIsSolveEquation(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%
%--------------------------------------------------------------------------

theEquationNames = cfdGetEquationNames;

isAvailable = false;
if find(strcmp(theEquationNames, theEquationName))
    isAvailable = true;
end