function phi = cfdInterpolateScalarFieldToFinerLevel(iLevel, PHI)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

theFineMesh = cfdGetCoarseMesh(iLevel-1);
theCoarseMesh = cfdGetCoarseMesh(iLevel);

theChildrenArray = cfdCoarseToFineAddressing(iLevel);

phi = zeros(theFineMesh.numberOfElements, 1);

for iCoarseElement=1:theCoarseMesh.numberOfElements
    for fineElementIndex=theChildrenArray
        phi(fineElementIndex) = PHI(iCoarseElement);
    end
end
