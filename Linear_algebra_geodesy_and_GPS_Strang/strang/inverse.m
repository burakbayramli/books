function X = inverse(A)
%INVERT Matrix inverse by Gauss Jordan elimination.
%	INVERSE(A) computes the inverse of the square matrix A
%	by computing the reduced echelon form, R, after A is
%	augmented by the identity matrix.
[n,n] = size(A);
I = eye(n,n);
R = ref([A I]);
X = R(:,n+1:n+n);
