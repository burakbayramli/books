function cfdCheckMesh
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     Apply a check to the mesh, giving out some important quantitative and
%     qualitative statistics
%--------------------------------------------------------------------------

% Check if mesh exists already in Region
global Region;

if isempty(Region)
    Region.caseDirectoryPath = pwd;
    
    % Read mesh files from directory
    
    % Initialize mesh
    cfdSetupMesh;
    
    % Define file paths
    caseDirectoryPath = cfdGetCaseDirectoryPath;
    
    pointsFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'points'];
    facesFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'faces'];
    ownerFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'owner'];
    neighbourFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'neighbour'];
    boundaryFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'boundary'];
    
    % Read Points
    cfdReadPointsFile(pointsFile);
    
    % Read Faces
    cfdReadFacesFile(facesFile);
    
    % Read owners
    cfdReadOwnerFile(ownerFile);
    
    % Read neighbours
    cfdReadNeighbourFile(neighbourFile);
    
    % Read boundaries
    cfdReadBoundaryFile(boundaryFile);
    
    % Topology
    cfdProcessTopology;
    
else
    
    if ~strcmp(Region.caseDirectoryPath, pwd)
        
        Region = struct;
        
        Region.caseDirectoryPath = pwd;
        
        % Read mesh files from directory
        
        % Initialize mesh
        cfdSetupMesh;
        
        % Define file paths
        caseDirectoryPath = cfdGetCaseDirectoryPath;
        
        pointsFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'points'];
        facesFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'faces'];
        ownerFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'owner'];
        neighbourFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'neighbour'];
        boundaryFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'boundary'];
        
        % Read Points
        cfdReadPointsFile(pointsFile);
        
        % Read Faces
        cfdReadFacesFile(facesFile);
        
        % Read owners
        cfdReadOwnerFile(ownerFile);
        
        % Read neighbours
        cfdReadNeighbourFile(neighbourFile);
        
        % Read cfdBoundaries
        cfdReadBoundaryFile(boundaryFile);
        
        % Topology
        cfdProcessTopology;
        
    end
end


cfdPrintMainHeader;

fprintf('Mesh stats\n');
fprintf('    points:           %d\n',Region.mesh.numberOfNodes);
fprintf('    faces:            %d\n',Region.mesh.numberOfFaces);
fprintf('    internal faces:   %d\n',Region.mesh.numberOfInteriorFaces);
fprintf('    cells:            %d\n',Region.mesh.numberOfElements);

% Determine faces per cell
facesPerCell = 0;
for iElement=1:Region.mesh.numberOfElements
    facesPerCell = facesPerCell + length(Region.mesh.elementFaces{iElement});
end
facesPerCell = facesPerCell/Region.mesh.numberOfElements;
fprintf('    faces per cell:   %d\n',facesPerCell);

fprintf('    boundary patches: %d\n',Region.mesh.numberOfBoundaryPatches);

fprintf('\nChecking geometry...\n');


% Domain cfdBounding box
maxLimit_x = -cfdBIG;
minLimit_x =  cfdBIG;

maxLimit_y = -cfdBIG;
minLimit_y =  cfdBIG;

maxLimit_z = -cfdBIG;
minLimit_z =  cfdBIG;


for iNode=1:Region.mesh.numberOfNodes
    maxLimit_x = max(Region.mesh.nodeCentroids(iNode,1), maxLimit_x);
    maxLimit_y = max(Region.mesh.nodeCentroids(iNode,2), maxLimit_y);
    maxLimit_z = max(Region.mesh.nodeCentroids(iNode,3), maxLimit_z);
    
    minLimit_x = min(Region.mesh.nodeCentroids(iNode,1), minLimit_x);
    minLimit_y = min(Region.mesh.nodeCentroids(iNode,2), minLimit_y);
    minLimit_z = min(Region.mesh.nodeCentroids(iNode,3), minLimit_z);
end


fprintf('    Overall domain cfdBounding box (%3.2f %3.2f %3.2f) (%3.2f %3.2f %3.2f)\n', minLimit_x, minLimit_y, minLimit_z, maxLimit_x, maxLimit_y, maxLimit_z);



% Boundary openness
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
theNumberOfFaces = cfdGetNumberOfFaces;

sumSf_b = sum(Region.mesh.faceSf(theNumberOfInteriorFaces+1:theNumberOfFaces,:));
if cfdMag(sumSf_b)<1e-12
    state = 'OK';
else
    state = 'FAIL';
end
fprintf('    Boundary openness (%f %f %f) %s.\n', sumSf_b(1), sumSf_b(2), sumSf_b(3), state);


% Cell openness
theNumberOfElements = cfdGetNumberOfElements;
elementFaces = cfdGetElementFaceIndices;
owners = cfdGetOwnersSubArrayForFaces;

faceOrientation = cell(theNumberOfElements,1);
for iElement=1:theNumberOfElements
    faceOrientation{iElement} = -1*ones(1,length(elementFaces{iElement}));
    for iFace=1:length(elementFaces{iElement})
        if owners(elementFaces{iElement}(iFace))==iElement
            faceOrientation{iElement}(iFace) = 1;            
        end
    end
end

maxCellOpenness = -cfdBIG;
for iElement=1:theNumberOfElements
    maxCellOpenness = max(norm(sum([Region.mesh.faceSf(elementFaces{iElement},1).*faceOrientation{iElement}',  ...
        Region.mesh.faceSf(elementFaces{iElement},2).*faceOrientation{iElement}', ...
        Region.mesh.faceSf(elementFaces{iElement},3).*faceOrientation{iElement}'])), maxCellOpenness);
end
if cfdMag(maxCellOpenness)<1e-12
    state = 'OK';
else
    state = 'FAIL';
end
fprintf('    Max cell openness = %f %s.\n', maxCellOpenness, state);


fprintf('\nEnd\n');

