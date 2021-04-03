function theMeshField = cfdSetupMeshField(theName, theType, varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function sets up mesh field
%--------------------------------------------------------------------------

% get mesh
theMesh = cfdGetMesh;

% Initialize and store field info
theMeshField.name = theName;

% Field type
theMeshField.type = theType;

if strcmp(theType, 'volScalarField')
    theInteriorArraySize = theMesh.numberOfElements;
    theBoundaryArraySize = theMesh.numberOfBElements;    
    theMeshField.phi     = cfdScalarList(theInteriorArraySize+theBoundaryArraySize);   
elseif strcmp(theType, 'volVectorField')
    theInteriorArraySize = theMesh.numberOfElements;
    theBoundaryArraySize = theMesh.numberOfBElements;    
    theMeshField.phi     = cfdVectorList(theInteriorArraySize+theBoundaryArraySize);       
elseif strcmp(theType, 'surfaceScalarField')
    theInteriorArraySize = theMesh.numberOfInteriorFaces;
    theBoundaryArraySize = theMesh.numberOfBFaces;    
    theMeshField.phi     = cfdScalarList(theInteriorArraySize+theBoundaryArraySize);    
elseif strcmp(theType, 'surfaceVector3Field')    
    theInteriorArraySize = theMesh.numberOfInteriorFaces;
    theBoundaryArraySize = theMesh.numberOfBFaces; 
    theMeshField.phi     = cfdVectorList(theInteriorArraySize+theBoundaryArraySize);       
end

% Previous iteration
theMeshField.prevIter.phi = theMeshField.phi;

% Previous time step
theMeshField.prevTimeStep.phi = theMeshField.phi;

% Store additional field settings
theNumberOfSubTerms = length(varargin);
for iSubTerm = 1:2:theNumberOfSubTerms
   theMeshField.(varargin{iSubTerm}) = varargin{iSubTerm+1};
end

% Store
cfdSetMeshField(theMeshField);

