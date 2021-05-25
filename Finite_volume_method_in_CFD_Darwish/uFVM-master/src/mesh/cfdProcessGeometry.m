function cfdProcessGeometry
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads poly mesh files from "constant/polyMesh" directory
%   and stores them in the database ~domain
%--------------------------------------------------------------------------

% Get info
theNumberOfElements = cfdGetNumberOfElements;
theNumberOfFaces = cfdGetNumberOfFaces;
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
theFaceNodesIndices = cfdGetFaceNodeIndices;
theNodeCentroids = cfdGetNodeCentroids;
theElementFaceIndices = cfdGetElementFaceIndices;
owners = cfdGetOwners;
neighbours = cfdGetNeighbours;

% Initialize mesh member arrays
elementCentroids = cfdVectorList(theNumberOfElements);
elementVolumes   = cfdScalarList(theNumberOfElements);
faceCentroids    = cfdVectorList(theNumberOfFaces);
faceSf           = cfdVectorList(theNumberOfFaces);
faceAreas        = cfdScalarList(theNumberOfFaces);
faceWeights      = cfdScalarList(theNumberOfFaces);
faceCF           = cfdVectorList(theNumberOfFaces);
faceCf           = cfdVectorList(theNumberOfFaces);
faceFf           = cfdVectorList(theNumberOfFaces);
wallDist         = cfdScalarList(theNumberOfFaces);
wallDistLimited  = cfdScalarList(theNumberOfFaces);

% Process basic face geometry
for iFace=1:theNumberOfFaces    
    theNodesIndices = theFaceNodesIndices{iFace};    
    theNumberOfFaceNodes = length(theNodesIndices);
    
    % Compute a rough centre of the face    
    local_centre = cfdVector(0,0,0);
    for iNode=theNodesIndices
        local_centre = local_centre + theNodeCentroids(iNode,:);
    end
    local_centre = local_centre/theNumberOfFaceNodes;
    
    centroid = cfdVector(0,0,0);
    Sf = cfdVector(0,0,0);
    area = 0;
    
    % Using the centre compute the area and centoird of vitual triangles
    % based on the centre and the face nodes    
    for iTriangle=1:theNumberOfFaceNodes
        point1 = local_centre;
        point2 = theNodeCentroids(theNodesIndices(iTriangle),:);
        if(iTriangle<theNumberOfFaceNodes)
            point3 = theNodeCentroids(theNodesIndices(iTriangle+1),:);
        else
            point3 = theNodeCentroids(theNodesIndices(1),:);
        end
        local_centroid = (point1+point2+point3)/3;
        local_Sf  = 0.5*cross(point2-point1,point3-point1);
        local_area = norm(local_Sf);
        
        centroid = centroid + local_area*local_centroid;
        Sf = Sf + local_Sf;
        area = area + local_area;
    end
    centroid = centroid/area;        
    %
    faceCentroids(iFace,:) = centroid;
    faceSf(iFace,:)        = Sf;
    faceAreas(iFace)       = area;
end

% Compute volume and centroid of each element
for iElement=1:theNumberOfElements
    
    theElementFaces = theElementFaceIndices{iElement};

    % Compute a rough centre of the element
    local_centre = cfdVector(0,0,0);
    for iFace=1:length(theElementFaces)
        faceIndex = theElementFaces(iFace);
        local_centre = local_centre + faceCentroids(faceIndex,:);
    end
    local_centre = local_centre/length(theElementFaces);
    
    % Using the centre compute the area and centoird of vitual triangles
    % based on the centre and the face nodes
    %
    localVolumeCentroidSum = cfdVector(0,0,0);
    localVolumeSum = 0;
    for iFace=1:length(theElementFaces)
        faceIndex = theElementFaces(iFace);                      
        
        Cf = faceCentroids(faceIndex,:) - local_centre;
                
        faceSign = -1;
        if iElement==owners(faceIndex)
            faceSign = 1;
        end
        
        local_Sf = faceSign*faceSf(faceIndex,:);
        
        % calculate face-pyramid volume
        localVolume = dot(local_Sf,Cf)/3;
        
        % Calculate face-pyramid centre
        localCentroid = 0.75*faceCentroids(faceIndex,:) + 0.25*local_centre;
        
        %Accumulate volume-weighted face-pyramid centre
        localVolumeCentroidSum = localVolumeCentroidSum + localCentroid*localVolume;
        
        % Accumulate face-pyramid volume
        localVolumeSum = localVolumeSum + localVolume;
    end
    %
    elementCentroids(iElement,:) = localVolumeCentroidSum/localVolumeSum;
    elementVolumes(iElement)     = localVolumeSum;
end

% Process secondary Face Geometry
for iFace=1:theNumberOfInteriorFaces   
    n = cfdUnit(faceSf(iFace,:));    
    %
    own = owners(iFace);
    nei = neighbours(iFace);        
    %
    faceCF(iFace,:)    = elementCentroids(nei,:) - elementCentroids(own,:);
    faceCf(iFace,:)    = faceCentroids(iFace,:)  - elementCentroids(own,:);
    faceFf(iFace,:)    = faceCentroids(iFace,:)  - elementCentroids(nei,:);
    faceWeights(iFace) = (faceCf(iFace,:)*n')/(faceCf(iFace,:)*n' - faceFf(iFace,:)*n'); % -(dot(faceFf(iFace,:),n))/(-dot(faceFf(iFace,:),n)+dot(faceCf(iFace,:),n))
end

for iBFace=theNumberOfInteriorFaces+1:theNumberOfFaces
    n = cfdUnit(faceSf(iBFace,:));    
    %
    own = owners(iBFace);       
    %
    faceCF(iBFace,:)        = faceCentroids(iBFace,:) - elementCentroids(own,:);
    faceCf(iBFace,:)        = faceCentroids(iBFace,:) - elementCentroids(own,:);
    faceWeights(iBFace)     = 1;
    wallDist(iBFace)        = max(dot(faceCf(iBFace,:),n), cfdSMALL);
    wallDistLimited(iBFace) = max(wallDist(iBFace), 0.05*cfdMag(faceCf(iBFace,:)));    
end

% Save and Store
mesh = cfdGetMesh;
%
mesh.elementCentroids = elementCentroids;
mesh.elementVolumes   = elementVolumes;
mesh.faceCentroids    = faceCentroids;
mesh.faceSf           = faceSf;
mesh.faceAreas        = faceAreas;
mesh.faceWeights      = faceWeights;
mesh.faceCF           = faceCF;
mesh.faceCf           = faceCf;
mesh.faceFf           = faceFf;
mesh.wallDist         = wallDist;
mesh.wallDistLimited  = wallDistLimited;
%
cfdSetMesh(mesh);
