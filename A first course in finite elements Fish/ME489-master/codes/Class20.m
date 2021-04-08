% ----------------------------------------------------------------------
%   Class 20 Matlab Code
%   Heat Transfer through a pipe using linear triangular elements
%   Developed by Adrian Buganza
%       email: buganza@purdue.edu
%   Converted by Emily Nadler
%       email: enadler@purdue.edu
%-----------------------------------------------------------------------

%% Read in a mesh 
fid = fopen('pipe_mesh.txt');
tline1 = fgetl(fid); % should just say "Nodes"
tline2 = fgetl(fid); % Number of nodes
n_node = str2double(tline2); % convert to double
node_XY = zeros(n_node,2); % read in node coordinates (x,y)
for i = 1:n_node
    tline = split(fgetl(fid)); % split at whitespace
    node_XY(i,1) = str2double(tline(1)); % convert to double and assign
    node_XY(i,2) = str2double(tline(2)); % convert to double and assign
end
tline = fgetl(fid); % should just say "Nodes"
n_elem = str2double(fgetl(fid));
elements = zeros(n_elem,3);

for i =1:n_elem
    tline = split(fgetl(fid));
    elements(i,1) = str2double(tline(1));
    elements(i,2) = str2double(tline(2));
    elements(i,3) = str2double(tline(3));
end
fprintf('node coordinates\n')
disp(node_XY)
fprintf('elements mapped to nodes\n')
disp(elements)

%% Build global stiffness
K = zeros(n_node);
GradN1e = zeros(1,2);
GradN2e = zeros(1,2);
GradN3e = zeros(1,2);

%% Loop over elements
for i = 1:n_elem
    % Get the nodes making up this element
    node1 = elements(i,1);
    node2 = elements(i,2);
    node3 = elements(i,3); 
    % Get the coordinates of the triangle
    x1 = node_XY(node1,1);
    y1 = node_XY(node1,2);
    x2 = node_XY(node2,1);
    y2 = node_XY(node2,2);
    x3 = node_XY(node3,1);
    y3 = node_XY(node3,2);
    
    % Call function to evaluate the gradient of shape functions 
    [GradN1e(1),GradN1e(2)] = Gradient_N1(x1,y1,x2,y2,x3,y3);
    [GradN2e(1),GradN2e(2)] = Gradient_N2(x1,y1,x2,y2,x3,y3);
    [GradN3e(1),GradN3e(2)] = Gradient_N3(x1,y1,x2,y2,x3,y3);
    % Build the element stiffness 
    A_ee = Area(x1,y1,x2,y2,x3,y3);
 Ke = A_ee*[dot(GradN1e,GradN1e),dot(GradN1e,GradN2e),dot(GradN1e,GradN3e);
            dot(GradN2e,GradN1e),dot(GradN2e,GradN2e),dot(GradN2e,GradN3e);
           dot(GradN3e,GradN1e),dot(GradN3e,GradN2e),dot(GradN3e,GradN3e)];
%     fprintf('Element %d \n',i)
%     disp(Ke)
    % Assemble into the global stiffness 
    K(node1,node1) = (Ke(1,1)+ K(node1,node1));
    K(node1,node2) = (Ke(1,2)+ K(node1,node2));
    K(node1,node3) = (Ke(1,3)+ K(node1,node3));
    K(node2,node1) = (Ke(2,1)+ K(node2,node1));
    K(node2,node2) = (Ke(2,2)+ K(node2,node2));
    K(node2,node3) = (Ke(2,3)+ K(node2,node3));
    K(node3,node1) = (Ke(3,1)+ K(node3,node1));
    K(node3,node2) = (Ke(3,2)+ K(node3,node2));
    K(node3,node3) = (Ke(3,3)+ K(node3,node3));
end % of the for loop
% In [32]:
%% Partition and solve
% solving the system with matrix partitioning, partion the K
n_E = 4;
n_F = n_node-n_E;
K_E = K(1:n_E,1:n_E);
K_F = K(n_E+1:end,n_E+1:end);
K_EF = K(1:n_E,n_E+1:end);
fprintf('K_E\n')
disp(K_E)
fprintf('K_F\n')
disp(K_F)
fprintf('K_EF\n')
disp(K_EF)

% known temperature
d_E = [0,0,100,100]';
% known external forces
F_F = zeros(n_F,1);
% solving A\b
d_F = linsolve(K_F,(F_F- (K_EF'*d_E))); %solve
%print(d_F)
% Assemble into a single vector
d = zeros(n_node,1);
d(1:n_E) = d_E;
d(n_E+1:end) = d_F;
disp(d)

% x = linspace(-2*pi,2*pi);
% y = linspace(0,4*pi);
% [X,Y] = meshgrid(x,y);
% Z = sin(X) + cos(Y);
% contourf(X,Y,Z,10)
% 
% Z =[node_XY(:,1),node_XY(:,2) , d]
% figure()
% %gca().set_aspect('equal')
% contourf(Z)
% colorbar()


%% To build the global stiffness matrix I need to have a function for the 
%% gradient of shape functions

function [dN1dx,dN1dy] = Gradient_N1(x1,y1,x2,y2,x3,y3)
    A_e1 = Area(x1,y1,x2,y2,x3,y3);
    dN1dx = (1./(2*A_e1))*(y2-y3);
    dN1dy = (1./(2*A_e1))*(x3-x2);
end
function [dN2dx,dN2dy] = Gradient_N2(x1,y1,x2,y2,x3,y3)
    A_e2 = Area(x1,y1,x2,y2,x3,y3);
    dN2dx = (1./(2*A_e2))*(y3-y1);
    dN2dy = (1./(2*A_e2))*(x1-x3);
    

end
function [dN3dx,dN3dy] = Gradient_N3(x1,y1,x2,y2,x3,y3)
    A_e3 = Area(x1,y1,x2,y2,x3,y3);
    dN3dx = (1./(2*A_e3))*(y1-y2);
    dN3dy = (1./(2*A_e3))*(x2-x1);

end
function A_e = Area(x1,y1,x2,y2,x3,y3)
    edge1 = [x2-x1,y2-y1,0.];
    edge2 = [x3-x1,y3-y1,0.];
    A_e = cross(edge1,edge2);
    A_e = norm(A_e)/2;

end
