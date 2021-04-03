function cfdAssembleConvectionDivergenceTerm(theEquationName,iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function claculates the convection divergence related fluxes
%--------------------------------------------------------------------------

% Get mesh
theMesh = cfdGetMesh;

% Get effective divergence
effDiv = cfdComputeEffectiveDivergence(theEquationName);

% Assemble
phi = cfdGetSubArrayForInterior(theEquationName, iComponent);

theCoefficients = cfdGetCoefficients;

theNumberOfElements = theMesh.numberOfElements;
for iElement=1:theNumberOfElements
    FLUXCE =   max(effDiv(iElement),0.0) - effDiv(iElement);
    FLUXVE = - max(effDiv(iElement),0.0) * phi(iElement);
    FLUXTE = FLUXCE * phi(iElement) + FLUXVE;

    theCoefficients.ac(iElement) = theCoefficients.ac(iElement) + FLUXCE;
    theCoefficients.bc(iElement) = theCoefficients.bc(iElement) - FLUXTE;        
end    

cfdSetCoefficients(theCoefficients);

end