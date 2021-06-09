%% Input Data from Figure 2.8
nsd	= 1;     % Number of spatial dimensions 
ndof= 1;     % Number of degrees-of-freedom per node
nnp	= 3;     % Total number of global nodes
nel	= 2;     % Total number of elements
nen	= 2;     % Number of nodes in each element
 
neq	= ndof*nnp;	% Number of equations
 
f	= zeros(neq,1);	% Initialize force vector
d	= zeros(neq,1);	% Initialize displacement vector
K	= zeros(neq); 	% Initialize stiffness matrix
 
% Element properties 
CArea	= [.5       1];   	% Elements cross-sectional area  
leng 	= [2        2];   	% Elements length
E       = [1        1];   	% Young’s Modulus

% prescribed displacements
d(1)	= 0;
nd	= 1;         	% Number of prescribed displacement degrees-of-freedom
 
% prescribed forces
f(3)	= 10;      	% Force at node 3 in the x-direction
 
% output controls
plot_truss  = 'yes';
plot_nod    = 'yes';
 
% mesh generation
truss_mesh_2_8;
