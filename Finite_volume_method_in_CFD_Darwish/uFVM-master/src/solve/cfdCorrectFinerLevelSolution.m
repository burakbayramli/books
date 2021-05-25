function cfdCorrectFinerLevelSolution(gridLevel)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Prolongate correction to finer level
%--------------------------------------------------------------------------

theCoefficients = cfdGetCoefficients(gridLevel);
DPHI = theCoefficients.dphi;

theFinerLevelCoefficients = cfdGetCoefficients(gridLevel-1);
dphi = theFinerLevelCoefficients.dphi;
theParents = theFinerLevelCoefficients.parents;
theNumberOfFineElements = theFinerLevelCoefficients.numberOfElements;

for iFineElement=1:theNumberOfFineElements
    iParent = theParents(iFineElement);
    dphi(iFineElement) = dphi(iFineElement) + DPHI(iParent);
end

% Store corrected correction
theFinerLevelCoefficients.dphi = dphi;
cfdSetCoefficients(theFinerLevelCoefficients,gridLevel-1);