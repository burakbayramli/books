function demo3
% Elliptic BVP for plates following BATOZ et al.
% DKT elements
% Exmple from BATOZ et AL. S. 1798
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOT USE of MATLAB PDE TOOLBOX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear, clc, format short, format compact
% -- Parameter ------------------------
E          = 0.7382e+06;
NU         = 0.3;
[KNOTEN,ELEMENTE,RB] = bsp03a;
%[KNOTEN,ELEMENTE,RB] = bsp03b;
%[KNOTEN,ELEMENTE,RB] = bsp03c;

N          = size(KNOTEN,2);
N3         =  3*N;
A          = sparse(N3,N3);
B          = zeros(N3,1);
% -- Dreieckelemente ---------------------------
for I = 1:size(ELEMENTE,2)
   K       = ELEMENTE(1:3,I);
   H       = ELEMENTE(4,I); P = ELEMENTE(5,I);
   XK      = KNOTEN(1,K); YK  = KNOTEN(2,K);
   %[SE,ME,BE,ecode] = fem_batoz(XK,YK,E,NU,H,P);
  % [SE,ME,BE,ecode] = fem_elstif(XK,YK,E,NU,H,P);
  % [SE,ME,BE,ecode] = fem_elstifa(XK,YK,E,NU,H,P);
   [SE,ME,BE,ecode] = fem_elstifb(XK,YK,E,NU,H,P);

   L       = ELEMENTE(1:3,I) - 1;
   M       = [1:3];
   K       = [3*L(1)+M, 3*L(2)+M, 3*L(3)+M];
   A(K,K)  = A(K,K) + SE;
   B(K)    = B(K) + BE;
end
% -- LAGER, nur homogene Dirichlet-Randbedingungen --
[P,Q]      = size(RB);
for I = 1:P
   L       = RB(I,1) - 1;
   for K = 1:3
      M    = 3*L + K;
      if RB(I,K+1) ~= 0
         B(M)   = 0; A(M,:) = 0; A(:,M) = 0; A(M,M) = 1;
      end
   end
end
% -- LGS ---------------------------------------
Z = A\B;
   Z1      = zeros(N,4);
   for i = 1:N
      Z1(i,:) = [i, Z(3*(i-1)+1), Z(3*(i-1)+2), Z(3*(i-1)+3)];
   end
   LOESUNG = Z1(1:N,2);
% -- Fuer Beispiel 3a mit exakten Werten -------------------
J = [25, 23, 21, 15, 13, 11];
BATOZ = LOESUNG(J,:);
EXAKT = [0.754, 0.518, 0.307, 0.328, 0.142, 0.056]'; % Nach Batoz
BATOZ_EXAKT = [BATOZ,EXAKT]
%LOESUNG
save daten1 LOESUNG KNOTEN ELEMENTE
