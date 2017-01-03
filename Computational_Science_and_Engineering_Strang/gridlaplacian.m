%2.4  gridlaplacian.m

%The Laplacian matrix L = A^T A for an N by N grid is created by kron.

B = toeplitz([2 -1 zeros(1,N-2)]); B(1,1) = 1; B(N,N) = 1;
L = kron(B,eye(N)) + kron(eye(N),B); % Section 3.5 explains kron.
