function cfdStoreMassFlowRate
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function accumulates mdot_f from total fluxes
%--------------------------------------------------------------------------

% Get info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
theNumberOfFaces = cfdGetNumberOfFaces;
iFaces = 1:theNumberOfInteriorFaces;
iBFaces = theNumberOfInteriorFaces+1:theNumberOfFaces;

owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;

owners_b = cfdGetOwnersSubArrayForBoundaryPatchFaces;

% Get pressure field
p = cfdGetSubArrayForInterior('p');
p_b = cfdGetSubArrayForAllBoundaryPatchFaces('p');

% Get fluxes
theFluxes = cfdGetFluxes;

% Interior faces
theFluxes.FluxTf(iFaces) = theFluxes.FluxCf(iFaces).*p(owners_f) + theFluxes.FluxFf(iFaces).*p(neighbours_f) + theFluxes.FluxVf(iFaces);

% Boundary faces
theFluxes.FluxTf(iBFaces) = theFluxes.FluxCf(iBFaces).*p(owners_b) + theFluxes.FluxFf(iBFaces).*p_b + theFluxes.FluxVf(iBFaces);

% Store
cfdSetFluxes(theFluxes);

% Get fields
theMdotField = cfdGetMeshField('mdot_f');
theMdotField.phi = theFluxes.FluxTf;
cfdSetMeshField(theMdotField);

