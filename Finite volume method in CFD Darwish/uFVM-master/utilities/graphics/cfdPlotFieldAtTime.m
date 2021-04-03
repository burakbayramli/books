function cfdPlotFieldAtTime(theFieldName, time, varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function plots the contour of the field at given time
%--------------------------------------------------------------------------

% Generate new figure
figure;

axis off
axis equal

% Get mesh and mesh info
theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;

if isempty(varargin)
    locale = 'volumes';
elseif length(varargin)==1
    locale = varargin{1};
end

theMeshField = cfdReadFieldFromTimeDirectory(theFieldName, num2str(time));

if strcmp(theMeshField.type, 'volVectorField')
    phi = cfdMag(theMeshField.phi);
else
    phi = theMeshField.phi;
end

% Convert cell f to matrix form
theFaceNodeIndices = cfdConvertCelltoMatrix(theFaceNodeIndices);

if strcmp(locale, 'nodes')
    cdata = cfdInterpolateFromElementsToNodes(phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'interp','EdgeColor','none');
else
    cdata = cfdInterpolateFromElementsToFaces('linear', phi);
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'flat');
end

colorbar;
title(theFieldName);
colormap jet;

% Adjusts to figure
rotate3d;
set(gca,'Clipping','off');
view(30,40);