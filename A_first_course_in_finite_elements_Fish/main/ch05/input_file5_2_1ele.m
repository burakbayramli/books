% Input Data for Example 5.2  (1 element)

nsd = 1;     % number of space dimensions 
ndof =1;     % number of degrees-of-freedom per node
nnp = 3;     % number of nodal points
nel = 1;     % number of elements
nen = 3;     % number of element nodes
 
neq = ndof*nnp;         % number of equations

f = zeros(neq,1);      % initialize nodal force vector
d = zeros(neq,1);      % initialize nodal displacement vector
K = zeros(neq);        % initialize stiffness matrix

flags = zeros(neq,1);  % initialize flag vector
e_bc = zeros(neq,1);   % initialize vector of essential boundary conditions
n_bc = zeros(neq,1);   % initialize vector of natural boundary conditions

% element characteristics (given at element nodes)
E      =  [8 8 8]';                % Young's modulus
body   =  [8 8 8]';                % body forces
CArea  =  [4 8 12]';               % cross-sectional area

% gauss integration
ngp    = 2;                          % number of gauss points

% essential B.C.'s (displacements or temperatures)
flags(1) = 2;                 % flags to mark nodes located for the essential boundary
e_bc(1) = 0;                  % value of essential B.C
nd      = 1;                  % number of nodes on the essential boundary
 
% natural B.C.'s (stresses or fluxes)
flags(3) = 1;                 % flags to mark nodes on the natural boundary
n_bc(3) = 0;                  % value of natural B.C
 
% point forces applied at any point 
P       = 24;       % array of point forces          
xp      = 5;        % array of coordinates where point forces are applied
np      = 1;        % number of point forces

% output plots
plot_bar    = 'yes';
plot_nod    = 'yes';
nplot       = nnp*10;  % number of points in the element to plot displacements and stresses


% mesh generation
bar_mesh5_2_1ele;




