function cfdUpdateFieldsForAllBoundaryPatches
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates fields at all boundary patches
%--------------------------------------------------------------------------

if cfdIsFieldAvailable('U')
    cfdUpdateVectorFieldForAllBoundaryPatches('U');
end

if cfdIsFieldAvailable('p')
    cfdUpdateScalarFieldForAllBoundaryPatches('p');
end

if cfdIsFieldAvailable('T')
    cfdUpdateScalarFieldForAllBoundaryPatches('T');
end

if cfdIsFieldAvailable('rho')
    cfdUpdateScalarFieldForAllBoundaryPatches('rho');
end

if cfdIsFieldAvailable('mu')
    cfdUpdateScalarFieldForAllBoundaryPatches('mu');
end

if cfdIsFieldAvailable('Cp')
    cfdUpdateScalarFieldForAllBoundaryPatches('Cp');
end

if cfdIsFieldAvailable('k')
    cfdUpdateScalarFieldForAllBoundaryPatches('k');
end