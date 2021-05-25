function cfdUpdateScales
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates the scales of all fields
%--------------------------------------------------------------------------

if cfdIsFieldAvailable('U')
    cfdUpdateScale('U');
end

if cfdIsFieldAvailable('p')
    cfdUpdateScale('p');
end

if cfdIsFieldAvailable('T')
    cfdUpdateScale('T');
end

if cfdIsFieldAvailable('rho')
    cfdUpdateScale('rho');
end