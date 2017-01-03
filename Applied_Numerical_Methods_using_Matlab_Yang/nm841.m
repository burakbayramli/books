%nm841 applies the Jacobi method 
% to find all the eigenvalues/eigenvectors of a symmetric matrix A.
clear
A= [2 0 1;0 -2 0;1 0 2];
EPS=1e-8;  MaxIter=100;
[L,V]= eig_Jacobi(A,EPS,MaxIter)
disp('Using eig()')
[V,LAMBDA]= eig(A) %modal matrix composed of eigenvectors
