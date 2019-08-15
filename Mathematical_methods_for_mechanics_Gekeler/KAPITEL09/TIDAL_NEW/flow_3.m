function VN1 = flow_3(p,t,V,VN,DTT,Parmeter);
% computes velocity and z
%%%%%%%%%%%%%%%%%%
%%% NO LUMPING %%%
%%%%%%%%%%%%%%%%%%
VN1 = VN; g = Parmeter(3); G6 = g/6; DT24 = DTT/24;
X    = p(1,:); Y = p(2,:); H = p(3,:); WN = VN(3,:) + H;
N    = size(p,2); M = size(t,2); MMM  = sparse(N,N);
RSU  = zeros(1,N); RSV = RSU; RSZ = RSU;
MM   = [2, 1, 1; 1, 2, 1; 1, 1, 2]; % MM*DE12 mass matrix
for I = 1:M
   K   = t(1:3,I);
   X21 = X(K(2)) - X(K(1)); X31 = X(K(3)) - X(K(1));
   X32 = X(K(3)) - X(K(2));
   Y21 = Y(K(2)) - Y(K(1)); Y31 = Y(K(3)) - Y(K(1));
   Y32 = Y(K(3)) - Y(K(2));
   DET = (X21*Y31 - X31*Y21); DET24 = DET/24;
   MMM(K,K) = MMM(K,K) + MM*DET24;
   C  = [-Y32;  Y31; -Y21]; D = [ X32; -X31;  X21];
   % -- U part --------------------
   BDT  = DTT*(VN(3,K)*C)*G6;
   AUXU = VN(1,K)*(VN(1,K)*C) + VN(2,K)*(VN(1,K)*D);
   SU   = (V(1,K) + sum(V(1,K)))*DET24 - DT24*(AUXU + sum(AUXU)) -BDT;
   % -- V part --------------------
   CDT  = DTT*(VN(3,K)*D)*G6;
   AUXV = VN(1,K)*(VN(2,K)*C) + VN(2,K)*(VN(2,K)*D);
   SV   = (V(2,K) + sum(V(2,K)))*DET24 - DT24*(AUXV + sum(AUXV)) -CDT;
   % -- Z part ------------------
   AUXZ1 = VN(1,K)*(WN(K)*C) + VN(2,K)*(WN(K)*D);
   AUXZ2 = (VN(1,K)*C + VN(2,K)*D)*WN(K);
   AUXZ = AUXZ1 + AUXZ2;
   SZ   = (V(3,K) + sum(V(3,K)))*DET24 - DT24*(AUXZ + sum(AUXZ));
   % -- Summieren -------------------------
   RSU(K)= RSU(K)+SU; RSV(K)= RSV(K)+SV; RSZ(K)= RSZ(K)+SZ;
end
B = [RSU',RSV',RSZ']; VN1 = MMM\B; VN1 = VN1';
