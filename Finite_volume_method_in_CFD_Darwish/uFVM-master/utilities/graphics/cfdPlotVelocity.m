function cfdPlotVelocity(varargin)
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     This function plots velocity cfdVector field
%--------------------------------------------------------------------------

% Generate new figure
figure;

axis off
axis equal

% Default properties
faceAlpha = 0.01;
edgeAlpha = 0.1;
cfdVectorSkip = 0;
cfdVectorScale = 0.5;
locale = 'volumes';

for iProperty=1:2:length(varargin)
    if strcmp(varargin{iProperty},'faceAlpha')
        faceAlpha = varargin{iProperty+1};
    elseif strcmp(varargin{iProperty},'edgeAlpha')
        edgeAlpha = varargin{iProperty+1};
    elseif strcmp(varargin{iProperty},'cfdVectorSkip')
        cfdVectorSkip = varargin{iProperty+1};
    elseif strcmp(varargin{iProperty},'cfdVectorScale')
        cfdVectorScale = varargin{iProperty+1};
    elseif strcmp(varargin{iProperty},'locale')
        locale = varargin{iProperty+1};
    end
end

% Get mesh and info
theNumberOfNodes = cfdGetNumberOfNodes;
theNumberOfFaces = cfdGetNumberOfFaces;
theNumberOfElements = cfdGetNumberOfElements;
theNumberOfBElements = cfdGetNumberOfBFaces;

theNodeCentroids = cfdGetNodeCentroids;
theFaceNodeIndices = cfdGetFaceNodeIndices;

% Get velocity field
theVelocityField = cfdGetMeshField('U');

% Fill in vertices anf faces arrays
for iNode=1:theNumberOfNodes
    v(iNode,:) = theNodeCentroids(iNode,:);
end

for iFace=1:theNumberOfFaces
    f{iFace,:} = theFaceNodeIndices{iFace};
end

% convert cell f to matrix form
f = cfdConvertCelltoMatrix(f);

% Plot mesh
cfdDrawMesh(v, f, 'FaceAlpha', faceAlpha, 'EdgeAlpha', edgeAlpha);

% Adjust perspective view
view(30,40);

% Plot cfdVectors over locale (volumes, nodes, or volumes plus cfdBoundary faces)
if strcmp(locale, 'volumes')
    % Define positions for cfdVectors
    theCentroids = cfdGetCentroidsForElements;
    
    x = theCentroids(:,1);
    y = theCentroids(:,2);
    z = theCentroids(:,3);
    
    vx = zeros(theNumberOfElements,1);
    vy = zeros(theNumberOfElements,1);
    vz = zeros(theNumberOfElements,1);
    
    if cfdVectorSkip==0
        for iElement=1:theNumberOfElements
            vx(iElement,1) = theVelocityField.phi(iElement,1);
            vy(iElement,1) = theVelocityField.phi(iElement,2);
            vz(iElement,1) = theVelocityField.phi(iElement,3);
        end
    else
        cfdVectorSkip = cfdVectorSkip + 1;
        for iElement=1:cfdVectorSkip:theNumberOfElements
            vx(iElement,1) = theVelocityField.phi(iElement,1);
            vy(iElement,1) = theVelocityField.phi(iElement,2);
            vz(iElement,1) = theVelocityField.phi(iElement,3);
        end
    end        
elseif strcmp(locale, 'volumesAndBFaces')
    % Define positions for cfdVectors
    theElementCentroids = cfdGetCentroidsForElements;
    theBElementCentroids = cfdGetFaceCentroidsSubArrayForAllBoundaryPatchFaces;    
    
    theCentroids = [theElementCentroids; theBElementCentroids];
    
    x = theCentroids(:,1);
    y = theCentroids(:,2);
    z = theCentroids(:,3);
    
    vx = zeros(theNumberOfElements+theNumberOfBElements,1);
    vy = zeros(theNumberOfElements+theNumberOfBElements,1);
    vz = zeros(theNumberOfElements+theNumberOfBElements,1);
    
    if cfdVectorSkip==0
        for iElement=1:theNumberOfElements+theNumberOfBElements
            vx(iElement,1) = theVelocityField.phi(iElement,1);
            vy(iElement,1) = theVelocityField.phi(iElement,2);
            vz(iElement,1) = theVelocityField.phi(iElement,3);
        end
    else
        cfdVectorSkip = cfdVectorSkip + 1;
        for iElement=1:cfdVectorSkip:theNumberOfElements+theNumberOfBElements
            vx(iElement,1) = theVelocityField.phi(iElement,1);
            vy(iElement,1) = theVelocityField.phi(iElement,2);
            vz(iElement,1) = theVelocityField.phi(iElement,3);
        end
    end    
end

% Add cfdVectors
hold on;
colormap jet;
cfdPlotVectors(x,y,z,vx,vy,vz,cfdVectorScale);

axis equal;

% Adjust axis limits to fit mesh scale
if min(cfdMag([vx vy vz]))~=max(cfdMag([vx vy vz]))
    caxis([min(cfdMag([vx vy vz])) max(cfdMag([vx vy vz]))]);
end

% Extra
colorbar;
rotate3d;
set(gca,'Clipping','off');


