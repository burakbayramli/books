function cfdUpdateGradients
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Update gradient of fields
%--------------------------------------------------------------------------

if cfdIsFieldAvailable('U')
    cfdUpdateGradient('U');
end

if cfdIsFieldAvailable('p')
    cfdUpdateGradient('p');
end

if cfdIsFieldAvailable('T')
    cfdUpdateGradient('T');
end

if cfdIsFieldAvailable('rho')
    cfdUpdateGradient('rho');
end
