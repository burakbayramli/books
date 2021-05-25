function cfdUpdateProperties
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates the properties that change throughout
%--------------------------------------------------------------------------

if cfdIsFieldAvailable('rho')
    cfdUpdateProperty('rho');
end

if cfdIsFieldAvailable('mu')
    cfdUpdateProperty('mu');
end

if cfdIsFieldAvailable('k')
    cfdUpdateProperty('k');
end

% Update drhodp if copressible
if cfdIsCompressible    
    cfdUpdateProperty('drhodp');
end





