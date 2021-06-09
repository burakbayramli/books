% Input Data for Example 2.2 
nsd 	= 2;	      % Number of space dimensions 
ndof 	= 2;     	  % Number of degrees-of-freedom per node
nnp 	= 3;    	  % Number of nodal points
nel 	= 2;     	  % Number of elements
nen 	= 2;     	  % Number of element nodes
 
neq 	= ndof*nnp;	  % Number of equations
 
f 	= zeros(neq,1);   % Initialize force vector
d 	= zeros(neq,1);   % Initialize displacement matrix
K 	= zeros(neq);     % Initialize stiffness matrix
 
% Element properties
CArea 	= [1       1   ];   	% Elements area  
leng  	= [1    sqrt(2)];   	% Elements length
phi   	= [90      45  ];   	% Angle
E     	= [1       1   ];   	% Young’s Modulus 
 
% prescribed displacements
% displacement     d1x    d1y    d2x    d2y
d           = [0      0      0      0]';
nd 	= 4; 	% Number of prescribed displacement degrees-of-freedom
 
% prescribed forces
f(5)	= 10;	   % Force at node 3 in the x-direction
f(6)	= 0;       % Force at node 3 in the y-direction
 
% output plots
plot_truss 	= 'yes';
plot_nod	= 'yes';

% mesh generation
truss_mesh_2_2;




