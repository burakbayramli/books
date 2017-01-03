function [V,F,P] = openOFF(filename, mode)
if nargin < 2
    mode = 'newplot';
end

% Load file
file  = fopen(filename,'r');
if file==-1
    warning(['file not found: ' filename]);
    V = zeros(0);
    F = zeros(0);
    P = zeros(0);
    return;
end
str = fscanf(file,'%s',1);

% Check Prefix
if strcmp(str, 'OFF') == false
    error('No OBJECT FILE FOMAT found');
end

% Read Data
numV = fscanf(file,'%u',1);
numF = fscanf(file,'%u',1);
numE = fscanf(file,'%u',1);

% Read 3D-vertices
V    = fscanf(file,'%f', [3,numV])';

% Read faces
poly = fscanf(file,'%u',1);
F_1  = fscanf(file,'%u',poly)';
F_r  = fscanf(file,'%u', [poly+1,numF])';
if norm(F_r(:,1)-poly)~=0
    error('OBJECT does not consist of similar polygons');
end
F    = [F_1; F_r(:,2:end)] + 1;
fclose(file);

%plot output
if strcmp(mode, 'newplot')
    figure;
end

if strcmp(mode, 'plot') | strcmp(mode, 'newplot')
    P = patch('Vertices', V, 'Faces', F, 'FaceVertexCData',0.3*ones(size(V,1),3));
    axis equal;
    shading interp;
    camlight right;
    camlight left;
end