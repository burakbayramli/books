function cfdUpdateScale(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates the scales of a field
%--------------------------------------------------------------------------

global Region;

phi = cfdGetDataArray(theFieldName);
phiMax = max(cfdMag(phi));
phiMin = min(cfdMag(phi));

if strcmp(theFieldName, 'p')
    vel_scale = cfdGetFieldScale('U');
    rho_scale = cfdGetFieldScale('rho');
    p_dyn = 0.5*rho_scale*vel_scale^2;
    phiScale = max(phiMax, p_dyn);
elseif strcmp(theFieldName, 'U')
    phiScale = max(cfdGeometricLengthScale,phiMax);
else
    phiScale = phiMax;
end

Region.fluid.(theFieldName).max = phiMax;
Region.fluid.(theFieldName).min = phiMin;
Region.fluid.(theFieldName).scale = phiScale;