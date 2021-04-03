function cfdPlotTransientField(theFieldName, varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function plots the contour of the field in transient mode
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

timeSteps = cfdGetTimeSteps;

phi = zeros(cfdGetNumberOfElements+cfdGetNumberOfBFaces, length(timeSteps));

iField = 1;
for timeStep=timeSteps'
    theMeshField = cfdReadFieldFromTimeDirectory(theFieldName, num2str(timeStep));
    if strcmp(theMeshField.type, 'volVectorField')
        phi(:,iField) = cfdMag(theMeshField.phi);
    else
        phi(:,iField) = theMeshField.phi;
    end
    iField = iField + 1;
end

phiMax = max(max(phi));
phiMin = min(min(phi));

% Convert cell f to matrix form
theFaceNodeIndices = cfdConvertCelltoMatrix(theFaceNodeIndices);

iField = 1;
for timeStep=timeSteps'    
    if iField==1
        if strcmp(locale, 'nodes')
            cdata = cfdInterpolateFromElementsToNodes(phi(:,iField));
            p = patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'interp','EdgeColor','none');
        else       
            cdata = cfdInterpolateFromElementsToFaces('linear', phi(:,iField));
            p = patch('vertices', theNodeCentroids, 'faces', theFaceNodeIndices, 'FaceVertexCData', cdata, 'FaceColor', 'flat');
        end        
    else
        if strcmp(locale, 'nodes')
            cdata = cfdInterpolateFromElementsToNodes(phi(:,iField));            
            set(p, 'CData', cdata);
            drawnow;            
        else       
            cdata = cfdInterpolateFromElementsToFaces('linear', phi(:,iField));           
            set(p, 'CData', cdata);
            drawnow;
        end 
    end
    
    caxis([phiMin, phiMax]);
    
    colorbar;
    title(theFieldName);
    colormap jet;
    
    % Adjusts to figure
    rotate3d;
    set(gca,'Clipping','off');
    view(30,40);
    
    iField = iField + 1;
end