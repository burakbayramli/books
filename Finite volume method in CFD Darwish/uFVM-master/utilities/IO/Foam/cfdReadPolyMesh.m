function cfdReadPolyMesh
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads poly mesh files from "constant/polyMesh" directory
%   and stores them in the database ~domain
%--------------------------------------------------------------------------

fprintf('\nReading Poly Mesh ...\n');

% Initialize mesh
cfdSetupMesh;

% Define file paths
caseDirectoryPath = cfdGetCaseDirectoryPath;

pointsFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'points'];
facesFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'faces'];
ownerFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'owner'];
neighbourFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'neighbour'];
cfdBoundaryFile = [caseDirectoryPath, filesep ,'constant', filesep,'polyMesh', filesep,'boundary'];

% Read Points
cfdReadPointsFile(pointsFile);

% Read Faces
cfdReadFacesFile(facesFile);

% Read owners
cfdReadOwnerFile(ownerFile);

% Read neighbours
cfdReadNeighbourFile(neighbourFile);

% Read cfdBoundaries
cfdReadBoundaryFile(cfdBoundaryFile);

% Check if cavity (i.e. no pressure prescribed boundaries)
cfdCheckIfCavity;

% Process topology
cfdProcessTopology;

end








