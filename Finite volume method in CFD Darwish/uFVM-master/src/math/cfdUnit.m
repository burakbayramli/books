function e = cfdUnit(v)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the cfdUnit cfdVector of a cfdVector/cfdVector list
%--------------------------------------------------------------------------

v_cfdMag = cfdMag(v);

e = [v(:,1)./v_cfdMag, v(:,2)./v_cfdMag, v(:,3)./v_cfdMag];

