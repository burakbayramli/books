function cfdAssembleMomentumDCSchemeTerm(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles deferred correction term
%--------------------------------------------------------------------------

% Correct for Deferred Correction
foamDict = cfdGetFoamDict;
theScheme = foamDict.fvSchemes.divSchemes.default;

if strcmp(theScheme,'Gauss upwind')
    return;
elseif strcmp(theScheme,'Gauss linear')
    % Second order upwind scheme
    processDCSOUScheme(iComponent);
else
    error([theScheme, ' divScheme incorrect\n']);
end

end

function processDCSOUScheme(iComponent)

% Get mesh info
theElementCentroids = cfdGetCentroidsForElements;
theFaceCentroids = cfdGetFaceCentroidsSubArrayForFaces;
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
 
owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;

iFaces = 1:theNumberOfInteriorFaces; 

iUpwind = pos.*owners_f + (1-pos).*neighbours_f;

% Get fields
phi = cfdGetSubArrayForInterior('U', iComponent);
gradPhi = cfdGetGradientSubArrayForInterior('U', iComponent);

mdot_f = cfdGetSubArrayForInterior('mdot_f');

% Get the upwind gradient at the interior faces
phiGradC = gradPhi(iUpwind,:, iComponent);

% Interpolated gradient to interior faces
phiGrad_f = cfdInterpolateGradientsFromElementsToInteriorFaces('Gauss linear corrected', gradPhi, phi);

rC = theElementCentroids(iUpwind,:);
rf = theFaceCentroids(iFaces,:);
rCf = rf - rC;

dc_corr = mdot_f .* dot(2*phiGradC' - phiGrad_f',rCf')';

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxTf(iFaces) = theFluxes.FluxTf(iFaces)  +  dc_corr;

cfdSetFluxes(theFluxes);

end

