clear all;
xyz=[5 15 15; 12 9 21; 10 3 12; 20 7 10];  % nodal coordinates
DNxi=[-1 1 0 0; -1 0 1 0; -1 0 0 1];  % derivatives of the master element shape functions
J=DNxi*xyz  % Jacobian
Jinv=inv(J)  % inverse of Jacobian
DNx=Jinv*DNxi   % derivatives of Ns with respect to x, y, z
T=[10 30 5 35]';  % nodal temperature
dTx=DNx*T         % temperature gradient
normal_vector=cross([-5 12 3]', [10 4 -2]');    % outward normal vector 
normal_vector=normal_vector./norm(normal_vector) % unit normal vector
dTdn=dTx'*normal_vector        % temperature gradient in the normal direction