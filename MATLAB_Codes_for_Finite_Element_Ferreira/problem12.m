clear
% E; modulus of elasticity
% I: second moment of area
% J: polar moment of inertia
% G: shear modulus
E = 210e9; A = 0.02;
Iy = 10e-5; Iz = 20e-5;J = 5e-5; G = 84e9;
% generation of coordinates and connectivities
nodeCoordinates = [0 0 0; 3 0 0; 0 0 -3; 0 -4 0];
xx = nodeCoordinates(:,1);
yy = nodeCoordinates(:,2);
zz = nodeCoordinates(:,3);
elementNodes = [1 2;1 3;1 4];
numberNodes = size(nodeCoordinates,1);
numberElements = size(elementNodes,1);

GDof = 6*numberNodes;
force = zeros(GDof,1);
stiffness = zeros(GDof,GDof);
% force vector
force(1) = -10e3;
force(3) = 15e3;
% stiffness matrix
[stiffness] = formStiffness3Dframe(GDof,numberElements, ...
				   elementNodes,numberNodes,nodeCoordinates,E,A,Iz,Iy,G,J);
% boundary conditions
prescribedDof = 7:24;
% solution
displacements = solution(GDof,prescribedDof,stiffness,force);
% displacements
disp('Displacements')
jj = 1:GDof; format long
f = [jj; displacements'];
fprintf('node U\n')
fprintf('%3d %12.8f\n',f)
