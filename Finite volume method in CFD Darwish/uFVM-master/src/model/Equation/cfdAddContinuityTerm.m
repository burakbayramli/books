function cfdAddContinuityTerm(term)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Set continuity equations terms
%--------------------------------------------------------------------------

theEquation = cfdGetModel('p');
theEquation.terms{end+1} = term;
cfdSetModel(theEquation);