function demo4
% -- Masterfile zur Plattenbiegung nach H.R. Schwarz --
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOT USE of MATLAB PDE TOOLBOX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nr = 1: beisp69.dat:
% Rechteck in achsenparalleler Lage, RBIPLA-Elemente
% nr = 2: beisp69.dat:
% Rechteck in achsenparalleler Lage, PAKPLA-Elemente
% nr = 3: beisp69.dat:
% Rechteck in achsenparalleler Lage, DRKPLA-Elemente
% diese elemente laufen nicht gut!
% nr = 4: beisp610.dat,
% Rechteck in schraeger Lage, PAKPLA-Elemente
% nr = 5: beisp610.dat,
% Rechteck in schraeger Lage, DRKPLA-Elemente
% nr = 6: einfaches Beispiel, PAKPLA-Elemente
% nr = 7: einfaches Beispiel, DRKPLA-Elemente
% nr = 8: Beispiel nach BATOZ DRKPLA_Elemente
% nr = 9: Beispiel nach BATOZ PAKPLA_Elemente

% N     : Anzahl Knotenvariable
% NKNOT : Anzahl Knotenpunkte
% TYP   : 1 = DRKPLA, 2 = PAKPLA, 3 = RIBPLA

clear, clc, format long, format compact
nr = 100; KK = [1,2,3,4,5,6,7,8,9];
while ~ismember(nr,KK)
   nr = input('Beispiel Nr. = ? (1/2/3/4/5/6/7/8/9) ');
end
[KNOTEN,ELEMENTE,RD,Parmeter] = bsp04(nr);
TYP = Parmeter(1); E = Parmeter(2); NU = Parmeter(3);
N = Parmeter(4); NKNOT = Parmeter(5);
A = sparse(N,N); B = zeros(N,1);
switch TYP
case 1, disp(' DRKPLA-Elemente (schlechte Ergebnisse!) ')
   for I = 1:size(ELEMENTE,2)
      H = ELEMENTE(4,I); P = ELEMENTE(5,I); K = ELEMENTE(1:3,:);
      X = KNOTEN(1,K); Y = KNOTEN(2,K);
      [SE,ME,BE,ecode] = fem_drkpla(X,Y,E,NU,H,P);
      L = ELEMENTE(1:3,I) - 1; M = [1:3];
      K = [3*L(1)+M, 3*L(2)+M, 3*L(3)+M];
      A(K,K) = A(K,K) + SE;
      B(K)   = B(K)   + BE;
     % I
     % ecode
   end
case 2, disp(' PAKPLA-Elemente ')
   for I = 1:size(ELEMENTE,2)
      M = [1,2,4]; H = ELEMENTE(5,I); P = ELEMENTE(6,I);
      K = ELEMENTE(M,I);
      X = KNOTEN(1,K); Y = KNOTEN(2,K);
      [SE,ME,BE,ecode] = fem_pakpla(X,Y,E,NU,H,P);
      L = ELEMENTE(1:4,I) - 1; M = [1:3];
      K = [3*L(1)+M, 3*L(2)+M, 3*L(3)+M, 3*L(4)+M];
      A(K,K) = A(K,K) + SE;
      B(K)   = B(K) + BE;
     % I
     % ecode
   end
case 3, disp(' RBIPLA-Elemente (nur achsenparallel) ')
   for I = 1:size(ELEMENTE,2)
      M = [1,2,4];  H = ELEMENTE(5,I);
      P = ELEMENTE(6,I); K = ELEMENTE(M,I);
      X = KNOTEN(1,K); Y = KNOTEN(2,K);
      [SE,ME,BE,ecode] = fem_rbipla(X,Y,E,NU,H,P);
      L = ELEMENTE(1:4,I) - 1; M = [1:4];
      K = [4*L(1)+M, 4*L(2)+M, 4*L(3)+M, 4*L(4)+M];
      A(K,K) = A(K,K) + SE;
      B(K)   = B(K)   + BE;
     % I
     % ecode
   end
end
% -- LAGER, nur homogene Randbedingungen --
P             = size(RD,1);
for I = 1:P
   L = RD(I,1) - 1;
   K = length(M)*L + RD(I,2);
   B(K) = 0; A(K,:) = 0; A(:,K) = 0; A(K,K) = 1;
end
% -- LGS ----------------------------------
R = chol(A); Y = - R'\B; Z = R\Y;
% -----------------------------------------
switch length(M)
case 3
   Z1 = zeros(NKNOT,4);
   for I = 1:NKNOT
      Z1(I,:) = [I, Z(3*(I-1)+1), Z(3*(I-1)+2), Z(3*(I-1)+3)];
   end
   LOESUNG = Z1(1:NKNOT,2)
case 4
   Z1 = zeros(NKNOT,5);
   for I = 1:NKNOT
      Z1(I,:) = [I, Z(4*(I-1)+1), Z(4*(I-1)+2),...
      Z(4*(I-1)+3), Z(4*(I-1)+4)];
   end
   LOESUNG = Z1(1:NKNOT,2)
end
LOESUNG = LOESUNG';
save daten4a LOESUNG KNOTEN ELEMENTE
