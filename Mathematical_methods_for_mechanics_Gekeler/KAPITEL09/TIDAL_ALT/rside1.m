function Y = rside1(T,XX,Parmeter,MASSMATRIX);
load daten2a p e t
g = Parmeter(3); g6 = g/6;
N = size(p,2); M = size(t,2);
U = XX(1:N); V = XX(N+1:2*N); Z = XX(2*N+1:3*N);
% -- Randwerte Einfuegen ---------------
[RDU,RDV,RDZ] = bsp02h(e,T,Parmeter);
U(RDU(1,:)) = RDU(2,:)';
V(RDV(1,:)) = RDV(2,:)';
Z(RDZ(1,:)) = RDZ(2,:)';
% ---------------------------
X = p(1,:); Y = p(2,:); H = p(3,:)'; W = Z + H;
RSU  = zeros(N,1); RSV = RSU; RSZ = RSU;
MM   = [2, 1, 1; 1, 2, 1; 1, 1, 2]/24; % MM*DE12 mass matrix
for I = 1:M
   K   = t(1:3,I);
   X21 = X(K(2)) - X(K(1)); X31 = X(K(3)) - X(K(1));
   X32 = X(K(3)) - X(K(2));
   Y21 = Y(K(2)) - Y(K(1)); Y31 = Y(K(3)) - Y(K(1));
   Y32 = Y(K(3)) - Y(K(2));
   C  = [-Y32,  Y31, -Y21];
   D  = [ X32, -X31,  X21];
   % -- U part: u_x u + u_y v - g z_x -------------
   AUXU = (C*U(K))*U(K) + (D*U(K))*V(K);
   SU   =  - MM*AUXU - g6*C*Z(K);
   % -- V part: v_x u + v_y v - g z_y  ------------
   AUXV = (C*V(K))*U(K) + (D*V(K))*V(K);
   SV   =  - MM*AUXV  - g6*D*Z(K);
   % -- Z part: w_xu + w_yv + (u_x + v_y)w ------
   AUXZ = (C*W(K))*U(K) + (D*W(K))*V(K) + (C*U(K) + D*V(K))*W(K);
   SZ   = - MM*AUXZ;
   % -- Summieren -------------------------
   RSU(K)= RSU(K) + SU;
   RSV(K)= RSV(K) + SV;
   RSZ(K)= RSZ(K) + SZ;
end
RSU = MASSMATRIX\RSU;
RSV = MASSMATRIX\RSV;
RSZ = MASSMATRIX\RSZ;
% -- Randwerte fuer Ableitungen Einfuegen -----
RSU(RDU(1,:)) = RDU(3,:)';
RSV(RDV(1,:)) = RDV(3,:)';
RSZ(RDZ(1,:)) = RDZ(3,:)';
Y = [RSU;RSV;RSZ];
