function LASTEN = rside_post(p,t,t1,U,V,RC)

D1 = [...
 -2, -1,  0,  3, -1,  1;
  1,  2,  0, -3, -1,  1;
  1, -1,  0,  0,  2, -2;
 -3,  3,  0,  0,  4, -4;
  1,  3,  0, -4,  8, -8;
 -3, -1,  0,  4,  8, -8]/30; %int_S Psi*Psi_x^T dxdy

D2 = [...
 -2,  0, -1,  1, -1,  3;
  1,  0, -1, -2,  2,  0;
  1,  0,  2,  1, -1, -3;
 -3,  0, -1, -8,  8,  4;
  1,  0,  3, -8,  8, -4;
 -3,  0,  3, -4,  4,  0]/30; %int_S Psi*Psi_y^T dxdy

LASTEN = zeros(length(U),1);
for I = 1:size(t,2)
   J = t(1:3,I); K = t1(1:3,I); L = [J;K];
   X = p(1,J); Y = p(2,J);
   X21 = X(2) - X(1); X31 = X(3) - X(1); X12 = - X21;
   Y21 = Y(2) - Y(1); Y31 = Y(3) - Y(1); Y12 = - Y21;
   DET = X21*Y31 - X31*Y21;
   LASTEN(L) = LASTEN(L) + D1*(V(L)*Y31 + U(L)*X31);
   LASTEN(L) = LASTEN(L) + D2*(V(L)*Y12 + U(L)*X12);
end
% -- gerade quadratische Randintegrale ------
N = length(U); B = zeros(N,1);
if ~isempty(RC)
for I = 1:size(RC,2)
   K       = RC([1,3],I);
   X       = p(1,K);
   Y       = p(2,K);
   [ME,BE] = fem_raqell(X,Y);
   L       = RC([1:3],I);
   %ALF     = RC(4,I);
   %BETA    = RC(5,I);
   %A(L,L)  = A(L,L) + ALF*ME;
     B(L)    = B(L)   + RC(5,I)*BE;
end
end
LASTEN = LASTEN + B;