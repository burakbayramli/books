function [S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1)
% E.W. Gekeler, Release 09.09.09 
% Calculation of matrices for Taylor-Hood element

N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
S = sparse(N,N); MM = S;  S_LIN = sparse(N1,N1);
C = sparse(N,N1); D = sparse(N,N1); BB = zeros(N1,1);
for I = 1:size(t,2)
   J = t(1:3,I); K = t1(:,I); L = [J;K];
   X = p(1,J); Y = p(2,J);
   [SE,ME,CE,DE,SE_LIN,ecode,BE] = thlsto(X,Y);
   S(L,L)     = S(L,L)  + SE;        % stiffness matrix for U resp. V
   C(L,J)     = C(L,J)  + CE;        % matrix corresp. DP/DX
   D(L,J)     = D(L,J)  + DE;        % matrix corresp. DP/DY
   MM(L,L)    = MM(L,L) + ME;        % mass matrix for U and V
   S_LIN(J,J) = S_LIN(J,J) + SE_LIN; % stiffness matrix of P
                                     % for stabilization
   BB(J) = BB(J) + BE; % for condition ''int p dxdy = 0''
end

