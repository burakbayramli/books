% Input Data for Example 5.3 from Chapter 5 (1 element - middle node at x=5)

nsd = 1;     % Number of space dimensions 
ndof =1;     % Number of degrees of freedom per node
nnp = 3;     % Number of nodal points
nel = 1;     % Number of elements
nen = 3;     % Number of element nodes
 
neq = ndof*nnp;         % Number of equations


f = zeros(neq,1);      % Initialize force vector
d = zeros(neq,1);      % Initialize displacement matrix
K = zeros(neq);        % Initialize stiffness matrix

flags = zeros(neq,1);   % Initialize flag vector
e_bc = zeros(neq,1);   % Initialize vector of essential boundary condition
n_bc = zeros(neq,1);   % Initialize vector of natural boundary condition

% Element Characteristics (given at the element nodes)
E      =  [8 8 8]';                % Young's modulus
body   =  [8 8 8]';                % body forces
CArea  =  [4 10 12]';              % Area function

% Gauss Integration
ngp    = 2;                          % Number of gauss points

% Essential B.C.'s (displacements or temperatures)
flags(1) = 2;                 % flags to indicate the nodes location for the essential B.C
e_bc(1) = 0;                  % value of essential B.C
nd      = 1;                  % number of essential B.C
 
% Natural B.C.'s (stresses or fluxes)
flags(3) = 1;                 % flags to indicate the nodes location for the natural B.C
n_bc(3) = 0;                  % value of natural B.C
nn      = 1;                  % number of natural B.C
 
% Point sources applied at a point (not at a node)
P       = 24;       % array of point forces          
xp      = 5;        % array of x coordinates at which point forces are applied
np      = 1;        % number of point sources

% Plots
plot_bar    = 'yes';
plot_nod    = 'yes';
nplot       = nnp*10;  % number of points to plot the displacement and stress


% Mesh Generation
bar_mesh5_3_1Aele;

