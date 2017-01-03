function [A,B,C,D] = network(F,G,H,K,Rnum,Rden);
% Synopsis: [A,B,C,D] = network(F,G,H,K,Rnum,Rden).
% Builds a state-space representation of a digital network.
% Input parameters:
% F, G, H, K: network connection matrices
% Rnum: rows contain numerator coefficients of blocks
% Rden: rows contain denominator coefficients of blocks
% Output parameters:
% A, B, C, D: state-space matrices.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

[L,Nnum] = size(Rnum); [L,Nden] = size(Rden);
A = []; B = []; C = []; D = []; N = 0;
for l = 1:L,
   rnum = Rnum(l,:); rden = Rden(l,:);
   while (rnum(length(rnum)) == 0), 
      rnum = rnum(1:length(rnum)-1); end
   while (rden(length(rden)) == 0), 
      rden = rden(1:length(rden)-1); end
   [At,Bt,Ct,Dt] = tf2ss(rnum,rden); Nt = length(Bt);
   A = [A,zeros(N,Nt); zeros(Nt,N),At];
   B = [B,zeros(N,1); zeros(Nt,l-1),Bt];
   C = [C,zeros(l-1,Nt); zeros(1,N),Ct];
   D = [D,zeros(l-1,1); zeros(1,l-1),Dt];
   N = N + Nt;
end
E = eye(L)-F*D;
if (rank(E) < L), error('Network is singular'), end
E = inv(E); A = A + B*E*F*C; B = B*E*G;
C = H*(eye(L) + D*E*F)*C; D = K + H*D*E*G;

