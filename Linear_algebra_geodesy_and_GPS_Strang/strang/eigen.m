function eigen(A)
%EIGEN	Describe eigenvalues and eigenvectors.
%	EIGEN(A) prints the eigenvalues
%	and eigenvectors of an n by n matrix.
%	If A is not diagonalizable, its single
%	eigenvector is printed twice.

disp(' ')
disp('The trace and determinant are:')
t = trace(A)
d = det(A)

disp(' ')
disp('det(e*I-A) is :')
disp(poly2str(poly(A),'e'))

[S,LAMBDA] = eig(A);
disp(' ')
disp('The matrix of eigenvalues is: ')
LAMBDA
disp(' ')
disp('The matrix of unit eigenvectors is: ')
S
