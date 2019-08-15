function P = pressure(p,e,t,U,V,NU)
%disp(' Uses MATLAB TOOLBOX ')

N = size(p,2);  
UE = U(1:N); VE = V(1:N);
VV = pdeintrp(p,t,[UE,VE]);
UE = UE.'; VE = VE.';
S4 = [2   1   1;  1   2   1;  1   1   2];
SB = [1;1;1];
N    = size(p,2); M = size(t,2); 
MM   = sparse(N,N); CC = MM; DD = MM; BB = zeros(N,1);
KKXX = MM; KKXY = MM; KKYY = MM;
KKXX = MM; KKXY = MM; KKYY = MM;
RUXX = zeros(N,1); RUXY = RUXX; RVYX = RUXX; RVYY = RUXX;
KKU = MM; KKV = MM;
C_AUX = zeros(M,3); D_AUX = zeros(M,3); DET = zeros(M,1);
X = p(1,:); Y = p(2,:);  
for I = 1:M
   K   = t(1:3,I); XA = X(K); YA = Y(K);
   X21 = XA(2)-XA(1); X31 = XA(3)-XA(1); X32 = XA(3)-XA(2);
   X12 = - X21; X13 = - X31; X23 = - X32;
   Y21 = YA(2)-YA(1); Y31 = YA(3)-YA(1); Y32 = YA(3)-YA(2);
   Y12 = - Y21; Y13 = -Y31; Y23 = - Y32;
   DET(I) = X21*Y31 - X31*Y21; 
   MM(K,K) = MM(K,K) + S4*DET(I)/24;
   BB(K)   = BB(K) + DET(I)*SB/6;
   C_AUX(I,:) = [Y23, Y31, Y12]; 
   D_AUX(I,:) = [X32, X13, X21];
   CC(K,K) = CC(K,K) + ones(3,1)*C_AUX(I,:)/6;
   DD(K,K) = DD(K,K) + ones(3,1)*D_AUX(I,:)/6;
   KXX  =  C_AUX(I,:).'*C_AUX(I,:)/(2*DET(I));
   KXY  =  C_AUX(I,:).'*D_AUX(I,:)/(2*DET(I));
   KYY  =  D_AUX(I,:).'*D_AUX(I,:)/(2*DET(I));

   KKXX(K,K) = KKXX(K,K) + KXX;
   KKXY(K,K) = KKXY(K,K) + KXY;
   KKYY(K,K) = KKYY(K,K) + KYY;
   KKU(K,K) =  KKU(K,K) + KXX*VV(1,I)   + KXY*VV(2,I);
   KKV(K,K) =  KKV(K,K) + KXY.'*VV(1,I) + KYY*VV(2,I);
end 

UX = CC*UE.'; UX = MM\UX; UY = DD*UE.'; UY = MM\UY; 
VX = CC*VE.'; VX = MM\VX; VY = DD*UE.'; VY = MM\VY; 

R2 = NU*(KKXX*UX + KKXY*UY + KKXY.'*VX + KKYY*VY);
RSIDES = - (KKU*UE.' + KKV*VE.') + R2;
KK  = KKXX + KKYY;
RSIDES = [RSIDES;0];
KK = [KK, BB;BB.',0];
P = KK\RSIDES; P = P(1:size(p,2));
