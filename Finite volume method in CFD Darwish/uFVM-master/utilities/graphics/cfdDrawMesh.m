function cfdDrawMesh(vertices, faces, varargin)
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     This function draws a mesh from inout vertices and faces
%--------------------------------------------------------------------------

% process input arguments
switch length(varargin)
    case 0 
        % default color is red
        varargin = {'facecolor', [1 0 0]};
    case 1
        % use argument as color for faces
        varargin = {'facecolor', varargin{1}};
    otherwise
        % otherwise add default settings before new options
        varargin = [{'facecolor', [1 0 0 ]} varargin];

end

% overwrites on current figure
hold on;

% if vertices are 2D points, add a z=0 coordinate
if size(vertices, 2) == 2
    vertices(1,3) = 0;
end

h = patch('vertices', vertices, 'faces', faces, varargin{:});
