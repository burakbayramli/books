% Input Data for Example Problem 8.1 (64 element mesh)

% Material property
k  = 5;        % thermal conductivity
D  = k*eye(2); % conductivity matrix

% Mesh specifications
nsd = 2;          % number of spacial dimensions
nnp = 81;         % number of total nodes
nel = 64;         % number of total elements
nen = 4;          % number of nodes on each element
ndof = 1;         % degrees-of-freedom per node
neq  = nnp*ndof;  % number of equations

f = zeros(neq,1);      % initialize nodal flux vector
d = zeros(neq,1);      % initialize nodal temperature vector
K = zeros(neq);        % initialize stiffness matrix

flags = zeros(neq,1);  % array to set B.C flags 
e_bc  = zeros(neq,1);  % essential B.C array
n_bc  = zeros(neq,1);  % natural B.C array
P    = zeros(neq,1);   % initialize point source defined at a node 
s     = 6*ones(nen,nel);  %  heat source defined over nodes

ngp    = 2;                          % number of gauss points

% essential BCs
nd = 17;     % number of nodes on essential boundary

% Essential B.C.
flags(1:9)     = 2;      e_bc(1:9)      = 0.0;
flags(10:9:73) = 2;      e_bc(10:9:73)  = 0.0;


% plots
compute_flux = 'yes';
plot_mesh    = 'yes';
plot_nod     = 'yes';
plot_temp    = 'yes';
plot_flux    = 'yes';



% natural B.C  - defined on edges positioned on natural boundary
n_bc    = [  73    74     75     76     77   78    79    80            %node 1
                   74    75     76     77     78   79    80    81      %node 2 
                  20    20     20      20    20   20    20    20       %flux value at node 1
                  20    20     20      20    20   20    20    20];     %flux value at node 2
nbe   = 8;         


% mesh generation
 mesh2d;

