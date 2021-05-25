function phi = cfdInterpolateFromNodesToElements(phi_n)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function interpolates the field from nodes to elements
%--------------------------------------------------------------------------

% Get field type
theNumberOfComponents = size(phi_n,2);

% Get info
theNumberOfElements = cfdGetNumberOfElements;

theNodeCentroids = cfdGetNodeCentroids;
theElementNodeIndices = cfdGetElementNodeIndices;
theElementCentroids = cfdGetCentroidsForElements;

% Initialize node array
phi = zeros(theNumberOfElements, theNumberOfComponents);

% Loop over elements
for iElement=1:theNumberOfElements
    % Current element properties
    C = theElementCentroids(iElement,:);    
    localNodeIndices = theElementNodeIndices{iElement};
        
    % Initialize
    localPhi = zeros(1,theNumberOfComponents);
    localInverseDistanceSum = 0;
    for nodeIndex=localNodeIndices
        N = theNodeCentroids(nodeIndex,:);
        d = cfdMag(C-N);
        
        localPhi_n = phi_n(nodeIndex,:);
        
        localPhi = localPhi + localPhi_n/d;
        localInverseDistanceSum = localInverseDistanceSum + 1/d;
    end    
    phi(iElement,:) = localPhi/localInverseDistanceSum;
end