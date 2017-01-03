function P = projmat(A)
%PROJMAT Projection matrix onto the column space.
%	P = projmat(A) is the symmetric, square matrix that
%	projects any vector onto the column space of A.

P = A*inverse(A'*A)*A';
