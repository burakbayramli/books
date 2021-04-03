function cfdAssembleDCSchemeTerm(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles deferred correction term for scalar equation
%--------------------------------------------------------------------------

% Correct for Deferred Correction
foamDict = cfdGetFoamDict;
theScheme = foamDict.fvSchemes.divSchemes.default;

if strcmp(theScheme,'Gauss upwind')
    return;
elseif strcmp(theScheme,'Gauss linear')
    % Second order upwind scheme
    processDCSOUScheme(theEquationName);
else
    error([theScheme, ' divScheme incorrect\n']);
end

end

function processDCSOUScheme(theEquationName)

% Get mesh info
theElementCentroids = cfdGetCentroidsForElements;
theFaceCentroids = cfdGetFaceCentroidsSubArrayForFaces;
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
 
owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;

iFaces = 1:theNumberOfInteriorFaces;  

% Get fields
phi = cfdGetSubArrayForInterior(theEquationName);
gradPhi = cfdGetGradientSubArrayForInterior(theEquationName);

mdot_f = cfdGetSubArrayForInterior('mdot_f');

pos = zeros(size(mdot_f));
pos(mdot_f>0) = 1;
iUpwind = pos.*owners_f + (1-pos).*neighbours_f;

% Get the upwind gradient at the interior faces
gradPhi_C = gradPhi(iUpwind,:);

% Interpolated gradient to interior faces
gradPhi_f = cfdInterpolateGradientsFromElementsToInteriorFaces('Gauss linear corrected', gradPhi, phi);

rC = theElementCentroids(iUpwind,:);
rf = theFaceCentroids(iFaces,:);
rCf = rf - rC;

% Calculate deferred correction
dc_corr = mdot_f .* dot(2*gradPhi_C' - gradPhi_f',rCf')';

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxTf(iFaces) = theFluxes.FluxTf(iFaces)  +  dc_corr;

cfdSetFluxes(theFluxes);

end

