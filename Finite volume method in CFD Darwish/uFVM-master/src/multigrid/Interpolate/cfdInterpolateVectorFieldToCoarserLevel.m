function PHI = cfdInterpolateVectorFieldToCoarserLevel(iLevel, phi)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

theFineMesh = cfdGetCoarseMesh(iLevel);
theCoarseMesh = cfdGetCoarseMesh(iLevel+1);

fineVolumes = [theFineMesh.elements.volume]';
coarseVolumes = [theCoarseMesh.elements.volume]';

theChildrenArray = cfdCoarseToFineAddressing(iLevel+1);

PHI = zeros(theCoarseMesh.numberOfElements, 3);

for iCoarseElement=1:theCoarseMesh.numberOfElements
    for fineElementIndex=theChildrenArray
        PHI(iCoarseElement, :) = PHI(iCoarseElement, :) + fineVolumes(fineElementIndex) * phi(fineElementIndex, :);
    end
    PHI(iCoarseElement, :) = PHI(iCoarseElement, :) / coarseVolumes(iCoarseElement);
end


