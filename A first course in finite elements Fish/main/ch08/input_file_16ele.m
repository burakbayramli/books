% Input Data for Example Problem 8.1 (16 element mesh)


% material property
k  = 5;        % thermal conductivity
D  = k*eye(2); % conductivity matrix

% mesh specifications
nsd  = 2;         % number of space dimensions
nnp  = 25;         % number of nodal nodes
nel  = 16;         % number of elements
nen  = 4;         % number of element nodes 
ndof = 1;         % degrees-of-freedom per node
neq  = nnp*ndof;  % number of equations



f = zeros(neq,1);      % initialize nodal source vector
d = zeros(neq,1);      % initialize nodal temperature vector
K = zeros(neq);        % initialize stiffness matrix

flags = zeros(neq,1);  % array to set B.C flags 
e_bc  = zeros(neq,1);  % essential B.C array
n_bc  = zeros(neq,1);  % natural B.C array
P    = zeros(neq,1);   % initialize point source defined at a node
s     = 6*ones(nen,nel);  % heat source

% gauss Integration
ngp    = 2;                          % number of gauss points

% boundary conditions and point forces
nd = 9;     % number of nodes on essential boundary

% essential B.C.
flags(1:5)    = 2;      e_bc(1:5)     = 0.0;
flags(6:5:21) = 2;      e_bc(6:5:21)  = 0.0;


% plots
compute_flux = 'yes';
plot_mesh    = 'yes';
plot_nod     = 'yes';
plot_temp    = 'yes';
plot_flux    = 'yes';



% natural B.C  - defined on edges positioned on natural boundary
n_bc    = [  21    22    23    24                % node 1
                   22    23    24    25          % node 2
                  20     20    20    20          % flux value at node 1 
                  20     20    20    20 ];       % flux value at node 2 
nbe = 4;


% mesh generation
 mesh2d;


 
 