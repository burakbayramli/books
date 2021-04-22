%% Input Data for Example 10.1

nsd	= 2;          % Number of spatial dimensions 
ndof= 2;          % Number of degrees-of-freedom per node
nnp	= 3;          % Total number of global nodes
nel	= 2;          % Total number of elements
nen	= 2;          % Number of nodes in each element
neq	= ndof*nnp;	  % Number of equations
neqe = ndof*nen;  % Number of equations for each element

f	= zeros(neq,1);	% Initialize force vector
d	= zeros(neq,1);	% Initialize displacement vector
K	= zeros(neq); 	% Initialize stiffness matrix

flags = zeros(neq,1); % initialize flag vector
e_bc = zeros(neq,1);  % initialize vector of essential boundary condition
n_bc = zeros(neq,1);  % initialize vector of natural boundary condition

% Element properties 
CArea	= [1     1 1]';     % Elements cross-sectional area  
leng 	= [8      4 ];      % Elements length
body   =  [-1     0 ]';     % body forces
E       = [1e4   1e4]';   	% Young’s Modulus

% gauss integration
ngp    = 2;                          % number of gauss points

% essential boundary conditions
% odd numbers for displacements; even numbers for rotations
flags(1) = 2; % flags to mark degrees-of-freedom located on the essential boundary
flags(2) = 2; % flags to mark degrees-of-freedom located on the essential boundary
e_bc(1) = 0;  % value of prescribed displacement
e_bc(2) = 0;  % value of prescribred rotation
nd = 2;       % number of degrees-of-freedom on the essential boundary
 
% natural boundary conditions
% odd numbers for shear forces; even numbers for moments
flags(5) = 1;  % flags to mark degrees-of-freedom located on the natural boundary
n_bc(5) = -20; % value of force
flags(6) = 1;  % flags to mark degrees-of-freedom located on the natural boundary
n_bc(6) = 20;  % value of moment

% Applied point forces 
P       = [-10 5]';        % array of point forces          
xp      = [4 8]'  ;        % array of coordinates where point forces are applied
np      =  2      ;        % number of point forces


% output controls
plot_beam  = 'yes';
plot_nod    = 'yes';
 
% mesh generation
beam_mesh_10_1;
% number of points for plot
nplot=300;
