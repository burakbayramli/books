function cfdAddTerm(theEquationName, theTermName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function adds the terms of the equation and stores them
%--------------------------------------------------------------------------

theEquation = cfdGetModel(theEquationName);

theTerm = setupTerm(theTermName);

cfdSetModel(theEquation);