clear

E = 210e9; A = 0.02;
Iy = 10e-5; Iz = 20e-5; J = 5e-5; G = 84e9;
% generation of coordinates and connectivities
nodeCoordinates = [0 0 0;
		   0 0 4;
		   4 0 4;
		   4 0 0;
		   0 5 0;
		   0 5 4;
		   4 5 4;
		   4 5 0];
xx = nodeCoordinates(:,1);
yy = nodeCoordinates(:,2);
zz = nodeCoordinates(:,3);
elementNodes = [1 5;2 6;3 7; 4 8; 5 6; 6 7; 7 8; 8 5];
numberNodes = size(nodeCoordinates,1);
numberElements = size(elementNodes,1);

GDof = 6*numberNodes;
force = zeros(GDof,1);
stiffness = zeros(GDof);
%force vector
force(37) = -15e3;
% stiffness matrix
[stiffness] = formStiffness3Dframe(GDof,numberElements, ...
				   elementNodes,numberNodes,nodeCoordinates,E,A,Iz,Iy,G,J);
% boundary conditions
prescribedDof = 1:24;
% solution
displacements = solution(GDof,prescribedDof,stiffness,force);
% displacements
disp('Displacements')
jj = 1:GDof; format long
f = [jj; displacements'];
fprintf('node U\n')
fprintf('%3d %12.8f\n',f)
% drawing mesh and deformed shape
U = displacements;
figure
XX = U(1:6:6*numberNodes);
YY = U(2:6:6*numberNodes);
