function Y = rside_1(XX,p,e,t,Parmeter)
% shallow water equations
% XX = [U;V;Z];
% Y  = right side of differential system (with sign)
% MM*XX_dot = Y;

A = Parmeter(1); PERIOD = Parmeter(2); g = Parmeter(3);
NU = Parmeter(4); kappa = Parmeter(5);
g6 = g/6;
N = size(p,2); M = size(t,2); 
U = XX(1,:); V = XX(2,:); Z = XX(3,:);
X = p(1,:); Y = p(2,:); W = Z + p(3,:);
RSU  = zeros(1,N); RSV = RSU; RSZ = RSU;
S1 = [1  -1   0; -1   1   0;  0   0   0];
S2 = [2  -1  -1; -1   0   1; -1   1   0];
S3 = [1   0  -1;  0   0   0; -1   0   1];
MM   = [2, 1, 1; 1, 2, 1; 1, 1, 2]/24; 
ME   = [2, 1; 1, 2]/6; 
for I = 1:M
   K   = t(1:3,I);
   X21 = X(K(2)) - X(K(1)); X31 = X(K(3)) - X(K(1));
   X32 = X(K(3)) - X(K(2));
   Y21 = Y(K(2)) - Y(K(1)); Y31 = Y(K(3)) - Y(K(1));
   Y32 = Y(K(3)) - Y(K(2));
   DET = X21*Y31 - X31*Y21;
   DET24 = DET/24; % mass matrix = MM*DET24

   C  = [-Y32;  Y31; -Y21]; D = [ X32; -X31;  X21];
   % -- U part: - (u_x u + u_y v + g z_x) -------------
   AUXU = U(K)*(U(K)*C) + V(K)*(U(K)*D);
   SU   = - AUXU*MM - g6*Z(K)*C;
   % -- V part: - (v_x u + v_y v + g z_y)  ------------
   AUXV = (V(K)*C)*U(K) + (V(K)*D)*V(K);
   SV   = - AUXV*MM - g6*Z(K)*D;
   % -- Z part: - (w_xu + w_yv + (u_x + v_y)w) ------
   AUXZ = (W(K)*C)*U(K) + (W(K)*D)*V(K) + (U(K)*C + V(K)*D)*W(K);
   SZ   = - AUXZ*MM;
   % -- Summieren -------------------
   CHEZ = kappa*sqrt(U(K).*U(K) + V(K).*V(K));
          % CHEZ = kappa*14;

   RSU(K) = RSU(K) + SU - (U(K).*CHEZ)*MM;
   RSV(K) = RSV(K) + SV - (V(K).*CHEZ)*MM;
   RSZ(K) = RSZ(K) + SZ;
   if NU ~= 0
      A1 =  (X31*X31 + Y31*Y31)/DET;
      B1 = -(X31*X21 + Y31*Y21)/DET;
      C1 =  (X21*X21 + Y21*Y21)/DET;
      KK = (A1*S1 + B1*S2 + C1*S3)/2;
      RSU(K)= RSU(K) - NU*U(K)*KK;
      RSV(K)= RSV(K) - NU*V(K)*KK;
   end
end
Y = [RSU;RSV;RSZ];
