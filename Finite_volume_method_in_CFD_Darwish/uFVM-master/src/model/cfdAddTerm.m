function cfdAddTerm(theEquationName, term)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Add equation term
%--------------------------------------------------------------------------

global Region;

Region.model.(theEquationName).terms{end+1} = term;