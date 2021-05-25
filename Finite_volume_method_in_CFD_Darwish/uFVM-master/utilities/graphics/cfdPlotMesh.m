function cfdPlotMesh(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function plots the mesh
%--------------------------------------------------------------------------

global Region;

if isempty(Region)
    % Initialize
    cfdStartSession;

    % Read OpenFOAM Files
    cfdReadOpenFoamFiles;
else
    if ~strcmp(Region.caseDirectoryPath, pwd)
        % Initialize
        cfdStartSession;

        % Read OpenFOAM Files
        cfdReadOpenFoamFiles;        
    end
end

% Generate new figure
figure;

axis off
axis equal

% Get mesh and info
theNumberOfNodes = cfdGetNumberOfNodes;
theNumberOfFaces = cfdGetNumberOfFaces;

theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;

% Fill in vertices anf faces arrays
for iNode=1:theNumberOfNodes
    v(iNode,:) = theNodeCentroids(iNode,:);
end

for iFace=1:theNumberOfFaces
    f{iFace,:} = theFaceNodeIndices{iFace};
end

% Convert cell f to matrix form
f = cfdConvertCelltoMatrix(f);

% Add face patches to figure
if(nargin>0)
    phiNodes = varargin{1};      
    h = patch('vertices', v, 'faces', f, 'FaceVertexCData',phiNodes,'FaceColor','interp', 'EdgeAlpha', .1);
else    
    phiNodes = zeros(length(v),1);
    h = patch('vertices', v, 'faces', f, 'FaceVertexCData', phiNodes,'FaceAlpha', 0.1, 'EdgeAlpha', .4);
end

% Adjusts to figure
rotate3d;
set(gca,'Clipping','off');
view(30,40)

end

