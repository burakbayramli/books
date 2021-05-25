function cfdReconstructRegion
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function re-constructs the Region by reading the existing data as
%   well latest results
%--------------------------------------------------------------------------

% Check if Region exists
global Region;

if ~isempty(Region)
    return;
end

% Create and store mesh
theMesh = cfdReadPolyMesh;
cfdSetMesh(theMesh);

% Read fields
timeSteps = cfdGetTimeSteps;
latestTimeStep = max(timeSteps);

theFoamFields = cfdReadTimeDirectory(num2str(latestTimeStep));
theFieldNames = fieldnames(theFoamFields);

Region.Step0.Elements.fields = {};
Region.Step0.Faces.fields = {};
Region.Step0.Nodes.fields = {};

for iField=1:length(theFieldNames)
    theFoamField = getfield(theFoamFields, theFieldNames{iField});
    theMeshField = cfdCreateMeshField(theFieldNames{iField}, theFoamField);
    cfdSetMeshField(theMeshField);    
end

