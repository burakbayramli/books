function cfdCorrectDensityField
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function corrects the density field
%--------------------------------------------------------------------------

% Get mesh info
p = cfdGetDataArray('p');
drhodp = cfdGetDataArray('drhodp');

% Update rho
theDensityField = cfdGetMeshField('rho');
theDensityField.phi = drhodp .* p;
cfdSetMeshField(theDensityField);
