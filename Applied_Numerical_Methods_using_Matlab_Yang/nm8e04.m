%nm8e04.m
A=[0 1;0.2 0.1]; B=[0; 2.2361]; % Eq.(E8.4-1)
[V,L]= eig(A) % V= modal matrix composed of eigenvectors (E8.4-2)
% L= diagonal matrix with eigenvalues on its diagonal
Ap=V^-1*A*V %diagonalize through similarity transformation (E8.4-3)
% into a diagonal matrix having the eigenvalues on the diagonal
Bp=V^-1*B % (E8.4-3)
