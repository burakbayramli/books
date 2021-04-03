function cfdSetContinuityTerms(terms)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Set continuity equations terms
%--------------------------------------------------------------------------

theEquation = cfdGetModel('p');
theEquation.terms = terms;
cfdSetModel(theEquation);