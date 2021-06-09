% Input Data for Example 5.2  (2 elements)

nsd = 1;     % number of space dimensions 
ndof =1;     % number of degrees-of-freedom per node
nnp = 5;     % number of nodal points
nel = 2;     % number of elements
nen = 3;     % number of element nodes
 
neq = ndof*nnp;         % number of equations


f = zeros(neq,1);      % initialize nodal force vector
d = zeros(neq,1);      % initialize nodal displacement matrix
K = zeros(neq);        % initialize stiffness matrix

flags = zeros(neq,1);  % initialize flag vector
e_bc = zeros(neq,1);   % initialize vector of essential boundary condition
n_bc = zeros(neq,1);   % initialize vector of natural boundary condition

% element characteristics (defined at each element node)
E      =  8*ones(nnp,1);                  % Young's modulus
body   =  8*ones(nnp,1);                  % body forces
CArea  =  [4     7    10    11    12]';   % cross-sectional area

% gauss integration
ngp    = 2;                          % number of gauss points

% essential B.C.'s (displacements or temperatures)
flags(1) = 2;                 % flags to mark the nodes located on essential boundary
e_bc(1) = 0;                  % value of essential B.C
nd      = 1;                  % number of essential B.C

% natural B.C.'s (stresses or fluxes)
flags(5) = 1;                 % flags to mark nodes located on natural boundary
n_bc(5) = 0;                  % value of natural B.C
 

% point forces 
P        = 24;      % array of point forces  
xp       = 5;       % array of  coordinates where point forces are applied
np       = 1;       % number of point forces

% output plots
plot_bar    = 'yes';
plot_nod    = 'yes';
nplot       = nnp*10;  % number of points in the element to plot displacements and stresses


% mesh generation
bar_mesh5_2_2ele;

