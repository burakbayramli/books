function cfdAddEnergyTerm(term)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Set energy equations terms
%--------------------------------------------------------------------------

theEquation = cfdGetModel('T');
theEquation.terms{end+1} = term;
cfdSetModel(theEquation);