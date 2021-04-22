% Input Data for Example Problem 8.1 (1 element mesh)

% material properties
k  = 5;        % thermal conductivity
D  = k*eye(2); % conduction matrix

% mesh specifications
nsd  = 2;         % number of space dimensions
nnp  = 4;         % number of nodal nodes
nel  = 1;         % number of elements
nen  = 4;         % number of element nodes 
ndof = 1;         % degrees-of-freedom per node
neq  = nnp*ndof;  % number of equations


f = zeros(neq,1);      % initialize nodal flux vector
d = zeros(neq,1);      % initialize nodal temperature vector vector
K = zeros(neq);        % initialize stiffness matrix

flags = zeros(neq,1);  % array to set B.C flags 
e_bc  = zeros(neq,1);  % essential B.C array
n_bc  = zeros(neq,1);  % natural B.C array
P    = zeros(neq,1);   % initialize point source defined at a node 
s     = 6*ones(nen,nel);  % constant heat source defined over the nodes

ngp    = 2;             % number of gauss points in each direction

% essential BCs
nd   = 3;         % number of nodes on essential boundary

% node:    1     2     3     4 
flags   = [2     2     2     0 ]';     % 


% what to plot
compute_flux = 'yes';
plot_mesh    = 'yes';
plot_nod     = 'yes';
plot_temp    = 'yes';
plot_flux    = 'yes';


% natural BC - defined on edges positioned on natural boundary
n_bc  = [ 4              %  node 1 
          1              %  node 2   
          20             %  flux at node 1
          20 ];           % flux at node 2
            
nbe = 1;   % number of edges on the natural boundary


% mesh generation
% node:  1    2    3    4
x   =  [0.0  0.0  2.0  2.0];     % X coordinate
y   =  [1.0  0.0  0.5  1.0];     % Y coordinate

IEN =  [2    3    4    1  ]';     % connectivity array

% function to plot the mesh
plotmesh;

