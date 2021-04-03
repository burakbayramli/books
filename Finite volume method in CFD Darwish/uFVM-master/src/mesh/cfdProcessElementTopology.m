function cfdProcessElementTopology
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function processes the topology of the elements
%--------------------------------------------------------------------------

% Info
theNumberOfElements = cfdGetNumberOfElements;
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
theNumberOfFaces = cfdGetNumberOfFaces;

owners = cfdGetOwnersSubArrayForFaces;
neighbours = cfdGetNeighboursSubArrayForFaces;
faceNodes = cfdGetFaceNodeIndices;


% Element-neighbours indices, Element-faces indices
elementNeighbours = cell(theNumberOfElements, 1);
elementFaces      = cell(theNumberOfElements, 1);

for iFace=1:theNumberOfInteriorFaces
    own = owners(iFace);
    nei = neighbours(iFace);
    %
    elementNeighbours{own} = [elementNeighbours{own} nei];
    elementNeighbours{nei} = [elementNeighbours{nei} own];
    %
    elementFaces{own} = [elementFaces{own} iFace];
    elementFaces{nei} = [elementFaces{nei} iFace];    
end

% Boundary patches
for iFace=theNumberOfInteriorFaces+1:theNumberOfFaces
    own = owners(iFace);
    %
    elementFaces{own} = [elementFaces{own} iFace];
end


% Element-node indices
elementNodes = cell(theNumberOfElements, 1);
for iElement=1:theNumberOfElements
    for faceIndex=elementFaces{iElement}'
        elementNodes{iElement} = [elementNodes{iElement} faceNodes{faceIndex}];
    end
    
    % Remove repetitions in node indices
    elementNodes{iElement} = unique(elementNodes{iElement});
end


% Element Anb coefficients indices
upperAnbCoeffIndex = cfdLabelList(theNumberOfInteriorFaces);
lowerAnbCoeffIndex = cfdLabelList(theNumberOfInteriorFaces);
for iElement=1:theNumberOfElements  
    iNb = 1;
    for faceIndex=elementFaces{iElement}
        
        % Skip if cfdBoundary face
        if faceIndex>theNumberOfInteriorFaces
            continue;
        end
        
        own = owners(faceIndex);
        nei = neighbours(faceIndex);        
        if iElement==own
            upperAnbCoeffIndex(faceIndex) = iNb;
        elseif iElement==nei
            lowerAnbCoeffIndex(faceIndex) = iNb;          
        end
        
        iNb = iNb + 1;
    end    
end


% Store
mesh = cfdGetMesh;
%
mesh.elementNeighbours  = elementNeighbours;
mesh.elementFaces       = elementFaces;
mesh.elementNodes       = elementNodes;
mesh.upperAnbCoeffIndex = upperAnbCoeffIndex;
mesh.lowerAnbCoeffIndex = lowerAnbCoeffIndex;
%
cfdSetMesh(mesh);