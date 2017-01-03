function [eigs,A]=eig_QR_Hs(A,kmax)
%Find eigenvalues by using QR factorization via Hesenberg
if nargin<2, kmax=200; end
Hs=hessenberg(A);
for k=1:kmax
   [Q,R]=qr_hessenberg(Hs); %Hs=Q*R; R=Q'*Hs=Q^-1*Hs
   Hs=R*Q; %Hs=Q^-1*Hs*Q
end
eigs=diag(Hs);
