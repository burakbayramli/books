function cfdInitializeMdotFromU
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Initialize mdot_f from U
%--------------------------------------------------------------------------

% Get associated fields
U = cfdGetDataArray('U');
rho = cfdGetDataArray('rho');

% Interpolate fields to faces
U_f = cfdInterpolateFromElementsToFaces('linear', U);
rho_f = cfdInterpolateFromElementsToFaces('linear', rho);

% Initialize mdot_f at interior faces
Sf = cfdGetFaceSfSubArrayForFaces;

% Calculate and store in data base
theMdotField = cfdGetMeshField('mdot_f');
theMdotField.phi = rho_f.*dot(Sf',U_f')';
cfdSetMeshField(theMdotField);
