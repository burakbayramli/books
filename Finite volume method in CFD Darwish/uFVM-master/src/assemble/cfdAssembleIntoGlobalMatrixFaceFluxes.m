function cfdAssembleIntoGlobalMatrixFaceFluxes
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles algebraic equation coefficients from the
%   contribution of the face fluxes of the current term of the equation
%--------------------------------------------------------------------------

% Get fluxes
theFluxes = cfdGetFluxes;

% Get info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
theNumberOfFaces = cfdGetNumberOfFaces;
owners = cfdGetOwnersSubArrayForFaces;
neighbours = cfdGetNeighboursSubArrayForFaces;

upperAnbCoeffIndex = cfdGetUpperAnbCoeffIndex;
lowerAnbCoeffIndex = cfdGetLowerAnbCoeffIndex;

% Get coefficients
theCoefficients = cfdGetCoefficients;

ac = theCoefficients.ac;
anb = theCoefficients.anb;
bc = theCoefficients.bc;

% Assemble fluxes of interior faces
for iFace=1:theNumberOfInteriorFaces
    own = owners(iFace);
    nei = neighbours(iFace);
    
    iOwnerNeighbourCoef = upperAnbCoeffIndex(iFace);    
    iNeighbourOwnerCoef = lowerAnbCoeffIndex(iFace);
    
    % Assemble fluxes for owner cell    
    ac(own)                       = ac(own)                       + theFluxes.FluxCf(iFace);
    anb{own}(iOwnerNeighbourCoef) = anb{own}(iOwnerNeighbourCoef) + theFluxes.FluxFf(iFace);
    bc(own)                       = bc(own)                       - theFluxes.FluxTf(iFace);
    
    % Assemble fluxes for neighbour cell
    ac(nei)                       = ac(nei)                       - theFluxes.FluxFf(iFace);
    anb{nei}(iNeighbourOwnerCoef) = anb{nei}(iNeighbourOwnerCoef) - theFluxes.FluxCf(iFace);
    bc(nei)                       = bc(nei)                       + theFluxes.FluxTf(iFace);
end

% Assemble fluxes of cfdBoundary faces
for iBFace=theNumberOfInteriorFaces+1:theNumberOfFaces
    own = owners(iBFace);
    
    % Assemble fluxes for owner cell
    ac(own) = ac(own) + theFluxes.FluxCf(iBFace);
    bc(own) = bc(own) - theFluxes.FluxTf(iBFace);
end

% Store
theCoefficients.ac = ac;
theCoefficients.anb = anb;
theCoefficients.bc = bc;

cfdSetCoefficients(theCoefficients);

end
