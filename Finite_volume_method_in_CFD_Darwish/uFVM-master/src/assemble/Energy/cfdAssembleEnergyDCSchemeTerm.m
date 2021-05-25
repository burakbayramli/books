function cfdAssembleEnergyDCSchemeTerm
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles deferred correction term for energy equation
%--------------------------------------------------------------------------

% Correct for Deferred Correction
foamDict = cfdGetFoamDict;
theScheme = foamDict.fvSchemes.divSchemes.default;

if strcmp(theScheme,'Gauss upwind')
    return;
elseif strcmp(theScheme,'Gauss linear')
    % Second order upwind scheme
    processDCSOUScheme;
else
    error([theScheme, ' divScheme incorrect\n']);
end

end

function processDCSOUScheme

% Get mesh info
theElementCentroids = cfdGetCentroidsForElements;
theFaceCentroids = cfdGetFaceCentroidsSubArrayForFaces;
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
 
owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;

iFaces = 1:theNumberOfInteriorFaces;  

iUpwind = pos.*owners_f + (1-pos).*neighbours_f;

% Get fields
T = cfdGetSubArrayForInterior('T');
gradT = cfdGetGradientSubArrayForInterior('T');

mdot_f = cfdGetSubArrayForInterior('mdot_f');

Cp = cfdGetSubArrayForInterior('Cp');
Cp_f = cfdInterpolateFromElementsToInteriorFaces('linear', Cp);

% Get the upwind gradient at the interior faces
gradT_C = gradT(iUpwind,:);

% Interpolated gradient to interior faces
gradT_f = cfdInterpolateGradientsFromElementsToInteriorFaces('Gauss linear corrected', gradT, T);

rC = theElementCentroids(iUpwind,:);
rf = theFaceCentroids(iFaces,:);
rCf = rf - rC;

% Calculate deferred correction
dc_corr = mdot_f .* Cp_f .* dot(2*gradT_C' - gradT_f',rCf')';

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxTf(iFaces) = theFluxes.FluxTf(iFaces)  +  dc_corr;

cfdSetFluxes(theFluxes);

end

