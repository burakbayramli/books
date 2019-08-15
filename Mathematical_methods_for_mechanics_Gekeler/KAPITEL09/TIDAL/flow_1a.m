function VN1 = flow_1a(p,t,V,VN,DTT,Parmeter);
% computes velocity and z
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LUMPED MASS MATRIX %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
VN1 = VN; g = Parmeter(3); G6 = g/6; DT24 = DTT/24;
NU = Parmeter(4);

X = p(1,:); Y = p(2,:); H = p(3,:); WN = VN(3,:) + H;
N = size(p,2); M = size(t,2);
S1 = [1  -1   0; -1   1   0;  0   0   0];
S2 = [2  -1  -1; -1   0   1; -1   1   0];
S3 = [1   0  -1;  0   0   0; -1   0   1];

RSU = zeros(1,N); RSV = RSU; RSZ = RSU; LUMP = RSU;
for I = 1:M
   K   = t(1:3,I);
   X21 = X(K(2)) - X(K(1)); X31 = X(K(3)) - X(K(1));
   X32 = X(K(3)) - X(K(2));
   Y21 = Y(K(2)) - Y(K(1)); Y31 = Y(K(3)) - Y(K(1));
   Y32 = Y(K(3)) - Y(K(2));
   FLAECHE = (X21*Y31 - X31*Y21)/2;
   DE12 = FLAECHE/12;
   LUMP(K) = LUMP(K) + FLAECHE/3; % lumped mass matrix
   C  = [-Y32;  Y31; -Y21];
   D  = [ X32; -X31;  X21];
   % -- U part --------------------
   BDT  = DTT*(VN(3,K)*C)*G6;
   AUXU = VN(1,K)*(VN(1,K)*C) + VN(2,K)*(VN(1,K)*D);
   SU   = (V(1,K) + sum(V(1,K)))*DE12 - DT24*(AUXU + sum(AUXU)) - BDT;
   % -- V part --------------------
   CDT  = DTT*(VN(3,K)*D)*G6;
   AUXV = VN(1,K)*(VN(2,K)*C) + VN(2,K)*(VN(2,K)*D);
   SV   = (V(2,K) + sum(V(2,K)))*DE12 - DT24*(AUXV + sum(AUXV)) - CDT;
   % -- z part ------------------
   AUXZ1 = VN(1,K)*(WN(K)*C) + VN(2,K)*(WN(K)*D);
   AUXZ2 = (VN(1,K)*C + VN(2,K)*D)*WN(K);
   AUXZ = AUXZ1 + AUXZ2;
   SZ   = (V(3,K) + sum(V(3,K)))*DE12 - DT24*(AUXZ + sum(AUXZ));
   % -- Summieren --------------------
   RSU(K)= RSU(K)+SU; RSV(K)= RSV(K)+SV; RSZ(K)= RSZ(K)+SZ;
      if NU ~= 0
       DET = X21*Y31 - X31*Y21; 
       A1 =  (X31*X31 + Y31*Y31)/DET;
      B1 = -(X31*X21 + Y31*Y21)/DET;
      C1 =  (X21*X21 + Y21*Y21)/DET;
      KK = (A1*S1 + B1*S2 + C1*S3)/2;
      RSU(K)= RSU(K) - NU*VN(1,K)*KK;
      RSV(K)= RSV(K) - NU*VN(2,K)*KK;
   end

end
VN1(1,:) = RSU./LUMP; VN1(2,:) = RSV./LUMP; VN1(3,:) = RSZ./LUMP;
