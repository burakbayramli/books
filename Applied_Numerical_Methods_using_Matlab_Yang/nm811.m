%nm811  to get the eigenvalues & eigenvectors of a matrix A.
clear
A=[0 1;0 -1];
[V,L]= eig(A) %V= modal matrix composed of eigenvectors
% L= diagonal matrix with eigenvalues on its diagonal
e=eig(A), roots(poly(A)) %just for eigenvalues
L=V^-1*A*V %diagonalize through similarity transformation 
% into a diagonal matrix having the eigenvalues on diagonal.
