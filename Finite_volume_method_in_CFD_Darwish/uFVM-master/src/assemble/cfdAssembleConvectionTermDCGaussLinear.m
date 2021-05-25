function theFluxes = cfdAssembleConvectionTermDCGaussLinear(theEquationName,theFluxes,iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function adds the second order upwind correction term to the total
%   face flux
%--------------------------------------------------------------------------

% get mesh info
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
iFaces = 1:numberOfInteriorFaces;   
iOwners = [theMesh.faces(iFaces).iOwner]';
iNeighbours = [theMesh.faces(iFaces).iNeighbour]';

theEquationMeshField = cfdGetMeshField(theEquationName);
phi = theEquationMeshField.phi;
phiGrad = theEquationMeshField.phiGradient;

theMdotField = cfdGetMeshField('mdot_f');
psi_f = theMdotField.phi(iFaces);

pos = zeros(size(psi_f));
pos(psi_f>0) = 1;

% Get the term coefficient
if strcmp(theEquationName, 'T')
    theSpecificHeatField = cfdGetMeshField('Cp');
    Cp = theSpecificHeatField.Cp(iFaces);    
    psi_f = psi_f .* Cp;
end

iUpwind = pos.*iOwners + (1-pos).*iNeighbours;

% Get the upwind gradient at the interior faces
phiGradC = phiGrad(iUpwind,:, iComponent);

% Interpolated gradient to interior faces
phiGradf = cfdInterpolateGradientsFromElementsToInteriorFaces('Gauss linear corrected', phiGrad, phi);

rC = [theMesh.elements(iUpwind).centroid]';
rf = [theMesh.faces(iFaces).centroid]';
rCf = rf - rC;

corr = psi_f .* dot(2*phiGradC' - phiGradf',rCf')';

theFluxes.FLUXTf(iFaces) = theFluxes.FLUXTf(iFaces) +  corr;
