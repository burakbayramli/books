function cfdProcessNodeTopology
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function processes the topology of the nodes
%--------------------------------------------------------------------------

elementNodes = cfdGetElementNodeIndices;
faceNodes    = cfdGetFaceNodeIndices;

% Invert connectivity
nodeElements = cfdInvertConnectivity(elementNodes);
nodeFaces    = cfdInvertConnectivity(faceNodes);

% Store
mesh = cfdGetMesh;
%
mesh.nodeElements = nodeElements;
mesh.nodeFaces    = nodeFaces;
%
cfdSetMesh(mesh);

