% Input Data for Example 5.2 (4 elements)

nsd = 1;     % number of space dimensions 
ndof =1;     % number of degrees-of-freedom per node
nnp = 9;     % number of nodal points
nel = 4;     % number of elements
nen = 3;     % number of element nodes
 
neq = ndof*nnp;         % number of equations


f = zeros(neq,1);      % initialize nodal force vector
d = zeros(neq,1);      % initialize nodal displacement matrix
K = zeros(neq);        % initialize stiffness matrix

flags = zeros(neq,1);  % initialize flags vector
e_bc = zeros(neq,1);   % initialize vector of essential boundary condition
n_bc = zeros(neq,1);   % initialize vector of natural boundary condition
P    = zeros(neq,1);   % initialize vector of point forces
 
% element characteristics (defined at element nodes)
E      =  8*ones(nnp,1);                % Young's modulus
body   =  8*ones(nnp,1);                % body forces
CArea  =  [4 5 6 7 8 9 10 11 12]';      % cross-sectional area

% gauss integration
ngp    = 2;                          % number of gauss points

% essential B.C.'s (displacements or temperatures)
flags(1) = 2;                 % flags to mark nodes located on the essential boundary
e_bc(1) = 0;                  % value of essential B.C
nd      = 1;                  % number of essential B.C

% natural B.C.'s (stresses or fluxes)
flags(nnp) = 1;                 % flags to mark nodes located on the natural boundary
n_bc(nnp) = 0;                  % value of natural B.C
 
% point forces 
P        = 24;      % array of point forces  
xp       = 5;       % array of  coordinates where point forces are applied
np       = 1;       % number of point forces

% output plots 
plot_bar    = 'yes';
plot_nod    = 'yes';
nplot       =  nnp*10;  % number of points in the element to plot displacements and stresses


% mesh generation
bar_mesh5_2_4ele;

