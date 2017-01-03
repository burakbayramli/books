function [eigs,A]=eig_QR(A,kmax)
%Find eigenvalues by using QR factorization
if nargin<2, kmax=200; end
for k=1:kmax
   [Q,R]=qr(A); %A=Q*R; R=Q'*A=Q^-1*A
   A=R*Q; %A=Q^-1*A*Q
end
eigs=diag(A);
