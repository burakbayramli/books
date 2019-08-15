function Y = rside10(T,XX,Parmeter,p,e,t,FF2,MASSMATRIX);
% WIE RSIDE, aber Randbedingung fuer Ableitungen
% NARGIN = 7: Rechte Seite fuer DGl. M*y' = F(t,y)
% NARGIN = 8: Rechte Seite fuer DGl. y' = inv(M)*F(t,y)
% Parmeter = [DT,A,H,L,PERIODE,g,N,TAU,XSCALE,YSCALE,NN,NU];

H = Parmeter(3); g = Parmeter(6); NU = Parmeter(12); g6 = g/6;
N = size(p,2); M = size(t,2); 
U = XX(1:N); V = XX(N+1:2*N); Z = XX(2*N+1:3*N);
% -- Randwerte Einfuegen ---------------
[RDU,RDV,RDZ] = feval(FF2,e,T,Parmeter);
U(RDU(1,:)) = RDU(2,:)'; V(RDV(1,:)) = RDV(2,:)';
Z(RDZ(1,:)) = RDZ(2,:)';
% ---------------------------
X = p(1,:); Y = p(2,:); W = Z + H;
RSU  = zeros(N,1); RSV = RSU; RSZ = RSU;
S1 = [1  -1   0; -1   1   0;  0   0   0];
S2 = [2  -1  -1; -1   0   1; -1   1   0];
S3 = [1   0  -1;  0   0   0; -1   0   1];

MM   = [2, 1, 1; 1, 2, 1; 1, 1, 2]/24; 
for I = 1:M
   K   = t(1:3,I);
   X21 = X(K(2)) - X(K(1)); X31 = X(K(3)) - X(K(1));
   X32 = X(K(3)) - X(K(2));
   Y21 = Y(K(2)) - Y(K(1)); Y31 = Y(K(3)) - Y(K(1));
   Y32 = Y(K(3)) - Y(K(2));
   C  = [-Y32,  Y31, -Y21]; D = [ X32, -X31,  X21];
   % -- U part: u_x u + u_y v - g z_x -------------
   AUXU = (C*U(K))*U(K) + (D*U(K))*V(K);
   SU   = - MM*AUXU - g6*C*Z(K);
   % -- V part: v_x u + v_y v - g z_y  ------------
   AUXV = (C*V(K))*U(K) + (D*V(K))*V(K);
   SV   = - MM*AUXV - g6*D*Z(K);
   % -- Z part: w_xu + w_yv + (u_x + v_y)w ------
   AUXZ = (C*W(K))*U(K) + (D*W(K))*V(K) + (C*U(K) + D*V(K))*W(K);
   SZ   = - MM*AUXZ;
   % -- Summieren -------------------------
   RSU(K)= RSU(K) + SU; RSV(K)= RSV(K)+ SV; RSZ(K)= RSZ(K) + SZ;
   if NU ~= 0
      DET = X21*Y31 - X31*Y21;
      A1 =  (X31*X31 + Y31*Y31)/DET;
      B1 = -(X31*X21 + Y31*Y21)/DET;
      C1 =  (X21*X21 + Y21*Y21)/DET;
      KK = (A1*S1 + B1*S2 + C1*S3)/2;
      RSU(K)= RSU(K) - NU*KK*U(K);
      RSV(K)= RSV(K) - NU*KK*V(K);
   end
end
Y = [RSU;RSV;RSZ];
if nargin == 8
   % all three equations are multiplied with inverse
   % of mass matrix
   flag = 1;   
   switch flag % no improvement in all cases
   case 1   
      BB = [RSU,RSV,RSZ]; 
      AUX = MASSMATRIX\BB;
      % Randwerte fuer Ableitungen
      AUX(RDU(1,:),1) = RDU(3,:).';
      AUX(RDV(1,:),2) = RDV(3,:).';
      AUX(RDZ(1,:),3) = RDZ(3,:).';
      Y = [AUX(:,1);AUX(:,2);AUX(:,3)];
   case 2        
      RSU(RDU(1,:)) = RDU(3,:).';
      RSV(RDV(1,:)) = RDV(3,:).';
      RSZ(RDZ(1,:)) = RDZ(3,:).';
      BB = [RSU,RSV,RSZ]; 
      AUX = MASSMATRIX\BB;
      Y = [AUX(:,1);AUX(:,2);AUX(:,3)];
   case 3   
      BB = [RSU,RSV,RSZ]; 
      AUX = MASSMATRIX\BB;
      Y = [AUX(:,1);AUX(:,2);AUX(:,3)];
   end   
end
