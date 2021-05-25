function cfdSetMomentumTerms(terms)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Set momentum equations terms
%--------------------------------------------------------------------------

theEquation = cfdGetModel('U');
theEquation.terms = terms;
cfdSetModel(theEquation);