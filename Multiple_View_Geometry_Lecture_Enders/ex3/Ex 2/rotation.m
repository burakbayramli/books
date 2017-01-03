function W = rotation(V,alpha,beta,gamma)

% Compute mean of vertices for vertex list V:
center = mean(V)

% Initialize W:
nVertices = size(V,1)
W = zeros(nVertices,3);

% Translation matrix (translates the model to the point (0,0,0)):
T = [eye(3,3), -center'; zeros(1,3), 1];

% Translation back to the center of V:
Tback = [eye(3,3), center'; zeros(1,3), 1];

% Rotation matrices in homegeneuous coordinates:
Rx = [1 0 0 0; 0 cos(deg2rad(alpha)) -sin(deg2rad(alpha)) 0; 0 sin(deg2rad(alpha)) cos(deg2rad(alpha)) 0; 0 0 0 1]
Ry = [cos(deg2rad(beta)) 0 sin(deg2rad(beta)) 0;  0 1 0 0;  -sin(deg2rad(beta))  0  cos(deg2rad(beta)) 0; 0 0 0 1]
Rz = [cos(deg2rad(gamma)) -sin(deg2rad(gamma)) 0 0; sin(deg2rad(gamma)) cos(deg2rad(gamma)) 0 0; 0 0 1 0; 0 0 0 1]

% Overall transformation matrix:
%G = Tback * Rz * Ry * Rx * T;
G = Tback * Rx * Ry * Rz * T;

% Homogeneous coordinates of V:
Vh = [V,ones(nVertices,1)];

% Rotate and translate the vertices:
Wh_t = G * Vh';
Wh = Wh_t';

% Back transform from homogenous to 3D coordinates:
W = Wh(:,1:3);


% type: 
% [V,F,P] = openOFF('model.off');
% W = rotation(V,50,0,25);
% P = patch('Vertices', W, 'Faces', F, 'FaceVertexCData',0.3*ones(size(W,1),3));
% axis equal;
% shading interp;
% camlight right;
% camlight left;