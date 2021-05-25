function phi_f = cfdInterpolateFromNodesToFaces(phi_n)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function interpolates the field from nodes to faces
%--------------------------------------------------------------------------

% Get field type
theNumberOfComponents = size(phi_n,2);

% Get info
theNumberOfFaces = cfdGetNumberOfFaces;

theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;
theFaceCentroids = cfdGetFaceCentroidsSubArrayForFaces;

% Initialize face array
phi_f = zeros(theNumberOfFaces, theNumberOfComponents);

for iFace=1:theNumberOfFaces
    
    % Current face properties
    localNodes = theFaceNodeIndices;    
    C = theFaceCentroids(iFace,:);
    
    localSumOfInverseDistance=0;
    localPhi = zeros(1,theNumberOfComponents);
    for nodeIndex=localNodes
        N = theNodeCentroids(nodeIndex,:);
        d = cfdMag(C-N);
        
        localPhi = localPhi + phi_n(nodeIndex,:)/d;
        localSumOfInverseDistance = localSumOfInverseDistance+1/d;
    end
    localPhi = localPhi/localSumOfInverseDistance;
    
    phi_f(iFace,:) = localPhi;
end