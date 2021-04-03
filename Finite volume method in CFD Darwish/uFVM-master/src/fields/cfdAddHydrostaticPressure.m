function cfdAddHydrostaticPressure
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function removes hydrostatic pressure from absolute pressure
%--------------------------------------------------------------------------

% Get info
g = cfdGetGravity;
rho = cfdGetDataArray('rho');
centroids = cfdGetCentroidsForElements;
centroids_b = cfdGetFaceCentroidsSubArrayForAllBoundaryPatchFaces;

theNumberOfElements = cfdGetNumberOfElements;

% Get absolute pressure field
thePressureField = cfdGetMeshField('p');

% Remove for interior
thePressureField.phi(1:theNumberOfElements) = thePressureField.phi(1:theNumberOfElements) + rho(1:theNumberOfElements).*(centroids*g');

% Remove for Boundaries
thePressureField.phi(theNumberOfElements+1:end) = thePressureField.phi(theNumberOfElements+1:end) + rho(theNumberOfElements+1:end).*(centroids_b*g');

% Store
cfdSetMeshField(thePressureField);