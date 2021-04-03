function cfdUpdateRHS(gridLevel,residual)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Calculate coarse level's RHS coefficients (bc)
%--------------------------------------------------------------------------

% Get info
theCoefficients = cfdGetCoefficients(gridLevel-1);
theParents = theCoefficients.parents;
theNumberOfElements = theCoefficients.numberOfElements;

theCoarseLevelCoefficients = cfdGetCoefficients(gridLevel);
theNumberOfCoarseElements = theCoarseLevelCoefficients.numberOfElements;

% Initialize coarse level RHS coefficient bc
BC = zeros(theNumberOfCoarseElements,1);

for iFineElement=1:theNumberOfElements    
    iParent = theParents(iFineElement);
    BC(iParent) = BC(iParent) + residual(iFineElement);
end

% Store
theCoarseLevelCoefficients.bc = BC;
cfdSetCoefficients(theCoarseLevelCoefficients,gridLevel);