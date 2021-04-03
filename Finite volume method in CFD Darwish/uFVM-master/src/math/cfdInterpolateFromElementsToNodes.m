function phi_n = cfdInterpolateFromElementsToNodes(phi)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function interpolates the field from elements to nodes
%--------------------------------------------------------------------------

% Get field type
theNumberOfComponents = size(phi,2);

% Get info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
theNumberOfElements = cfdGetNumberOfElements;
theNumberOfNodes = cfdGetNumberOfNodes;

theNodeCentroids = cfdGetNodeCentroids;
theNodeFaceIndices = cfdGetNodeFaceIndices;
theNodeElementIndices = cfdGetNodeElementIndices;

theElementCentroids = cfdGetCentroidsForElements;
theFaceCentroids = cfdGetFaceCentroidsSubArrayForFaces;

% Initialize node array
phi_n = zeros(theNumberOfNodes, theNumberOfComponents);

% Loop over nodes
for iNode=1:theNumberOfNodes
    
    % Current node properties
    N = theNodeCentroids(iNode,:);
    
    nodeFaces = theNodeFaceIndices{iNode};
    nodeElements = theNodeElementIndices{iNode};

    % Initialize
    localPhiNode = zeros(1,theNumberOfComponents);
    localInverseDistanceSum = 0;

    if isempty(nodeFaces(nodeFaces>theNumberOfInteriorFaces))
        localElementIndices = nodeElements;

        for iElement = localElementIndices
            C = theElementCentroids(iElement,:);

            d = cfdMag(N-C);
            localPhi = phi(iElement);

            localPhiNode = localPhiNode + localPhi/d;
            localInverseDistanceSum = localInverseDistanceSum + 1/d;       
        end
    else 
        localBFacesIndices = nodeFaces(nodeFaces>theNumberOfInteriorFaces);
        for iBFace = localBFacesIndices            
            C = theFaceCentroids(iBFace,:);
            iBElement = theNumberOfElements+(iBFace-theNumberOfInteriorFaces);

            d = cfdMag(N-C);
            localPhi = phi(iBElement);

            localPhiNode = localPhiNode + localPhi/d;
            localInverseDistanceSum = localInverseDistanceSum + 1/d;       
        end
        
    end
    phi_n(iNode,:) = localPhiNode/localInverseDistanceSum;
end
