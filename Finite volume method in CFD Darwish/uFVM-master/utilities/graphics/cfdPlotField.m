function cfdPlotField(theFieldName, varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function plots the contour of the field
%--------------------------------------------------------------------------

% Generate new figure
figure;

axis off
axis equal

% Get mesh and mesh info
theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;

phi = cfdGetDataArray(theFieldName);
type = cfdGetFieldType(theFieldName);

if nargin<2
    locale = 'volumes';
    limits = [];
elseif nargin<3
    locale = varargin{1};
    limits = [];
else
    locale = varargin{1};
    limits = varargin{2};
end

if strcmp(locale, 'nodes')
    
    % Convert cell f to matrix form
    theFaceNodeIndices = cfdConvertCelltoMatrix(theFaceNodeIndices);
    
    if strcmp(type, 'volVectorField')
        cdata = cfdInterpolateFromElementsToNodes(cfdMag(phi));
    else
        cdata = cfdInterpolateFromElementsToNodes(phi);
    end
    
    % Add face patches to figure
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'interp', 'EdgeColor','none');
else
    
    % Convert cell f to matrix form
    theFaceNodeIndices = cfdConvertCelltoMatrix(theFaceNodeIndices);
    
    if strcmp(type, 'volVectorField')
        cdata = cfdInterpolateFromElementsToFaces('linear', cfdMag(phi));
    else
        cdata = cfdInterpolateFromElementsToFaces('linear', phi);
    end
    
    % Add face patches to figure
    patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'flat');
end

if ~isempty(limits)
    set(gca,'CLim', limits)
end

colorbar;
title(theFieldName);
colormap jet;

% Adjusts to figure
rotate3d;
set(gca,'Clipping','off');
view(30,40)