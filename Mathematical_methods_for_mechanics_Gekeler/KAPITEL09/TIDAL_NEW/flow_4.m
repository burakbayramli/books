function VN1 = flow_5(p,t,V,VN,DT,Parmeter);
% wie flow_2.m aber mit Eddy-Visk.
% computes velocity and z
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SELECTIVE LUMPING %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
VN1 = VN; g = Parmeter(3); G6 = g/6; 
NU = Parmeter(4);
eeps = 0.9; % selective lumping parammeter
X = p(1,:); Y = p(2,:); H = p(3,:); WN = VN(3,:) + H;
N = size(p,2); M = size(t,2);
RSU = zeros(1,N); RSV = RSU; RSZ = RSU; LUMP = RSU;
MM = [2, 1, 1; 1, 2, 1; 1, 1, 2]; % MM*DET/24 mass matrix
S1 = [1  -1   0; -1   1   0;  0   0   0];
S2 = [2  -1  -1; -1   0   1; -1   1   0];
S3 = [1   0  -1;  0   0   0; -1   0   1];
MM = [2   1  1;  1   2   1; 1   1   2]/24; % mass matrix = MM*DET

for I = 1:M
   K   = t(1:3,I);
   X21 = X(K(2)) - X(K(1)); X31 = X(K(3)) - X(K(1));
   X32 = X(K(3)) - X(K(2));
   Y21 = Y(K(2)) - Y(K(1)); Y31 = Y(K(3)) - Y(K(1));
   Y32 = Y(K(3)) - Y(K(2));
   DET = X21*Y31 - X31*Y21; DET24 = DET/24;
   LUMP(K) = LUMP(K) + DET/6; % lumped mass matrix
   MME = eeps*DET*eye(3)/6 + (1 - eeps)*MM*DET;
   C  = [-Y32;  Y31; -Y21]; D = [ X32; -X31;  X21];
   % -- U part --------------------
   BDT  = (VN(3,K)*C)*G6;
   AUXU = VN(1,K)*(VN(1,K)*C) + VN(2,K)*(VN(1,K)*D);
   if NU == 0
      SU = V(1,K)*MME - DT*(AUXU*MM + BDT); % selective lumping
   else
      SU = V(1,K)*DET/6 - DT*(AUXU*MM + BDT); % simple lumping
   end
   % -- V part --------------------
   CDT  = (VN(3,K)*D)*G6;
   AUXV = VN(1,K)*(VN(2,K)*C) + VN(2,K)*(VN(2,K)*D);
   if NU == 0
      SV = V(2,K)*MME - DT*(AUXV*MM + CDT); % selective lumping
   else
      SV = V(2,K)*DET/6 - DT*(AUXV*MM + CDT); % simple lumping
   end   
   % -- Z part ------------------
   AUXZ1 = VN(1,K)*(WN(K)*C) + VN(2,K)*(WN(K)*D);
   AUXZ2 = (VN(1,K)*C + VN(2,K)*D)*WN(K);
   AUXZ = AUXZ1 + AUXZ2;
   SZ   = V(3,K)*MME - DT*AUXZ*MM;
   
   %SZ   = V(3,K)*DET/6 - DT*AUXZ*MM;

   % -- Summieren -------------------
   RSU(K)= RSU(K)+SU; RSV(K)= RSV(K)+SV; RSZ(K)= RSZ(K)+SZ;
   if NU ~= 0
       A1 =  (X31*X31 + Y31*Y31)/DET;
      B1 = -(X31*X21 + Y31*Y21)/DET;
      C1 =  (X21*X21 + Y21*Y21)/DET;
      KK = (A1*S1 + B1*S2 + C1*S3)/2;
      RSU(K)= RSU(K) - NU*VN(1,K)*KK;
      RSV(K)= RSV(K) - NU*VN(2,K)*KK;
   end
end
VN1(1,:) = RSU./LUMP; VN1(2,:) = RSV./LUMP; VN1(3,:) = RSZ./LUMP;
