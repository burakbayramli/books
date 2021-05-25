function cfdSetTerms(theEquationName, terms)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Set equation terms
%--------------------------------------------------------------------------

theEquation = cfdGetModel(theEquationName);
theEquation.terms = terms;
cfdSetModel(theEquation);