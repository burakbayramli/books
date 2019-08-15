function [t,q] = mesh12(p,e,SEGNR1,SEGNR2);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% schoepft ein einfach zusammenhaengendes Gebiet durch
% Quadrate aus und den Rest  durch Dreiecke
% OUTPUT:
%       t Dreieckselemente
%       q Quadrate
TOL = 1E-8;  % !!!!!!!!!!!!!!!!!!!!
% -- Triangulierung ---------------------
t  = delaunay(p(1,:),p(2,:));
t  = t';
t1 = test02(p,t); % ev. Reihenfolge aendern
t  = mesh27(p,e,t1,SEGNR1,SEGNR2);
% ---------------------------------------
[RDKN,t1,t2] = mesh23(p,e,t,SEGNR1);
% Maximale Seitenlaengen berechnen ------------------------
EL1  = [t2; t2(1:2,:)];
DIFF = p(:,EL1(1,:)) - p(:,EL1(2,:));
L1   = DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:);
L1   = sqrt(L1);
DIFF = p(:,EL1(2,:)) - p(:,EL1(3,:));
L2   = DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:);
L2   = sqrt(L2);
DIFF = p(:,EL1(3,:)) - p(:,EL1(1,:));
L3   = DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:);
L3   = sqrt(L3);
EL1  = [EL1; L1; L2; L3];
N    = size(EL1,2);
MAXL = zeros(N,1);
for I = 1:N
   MAXL(I) = max(EL1(6:8,I));
end
% -- Dreiecke mit gemeinsamer max. Seitenlange
EL2  = zeros(3,N);
for I =1:N
   J        = find(EL1(6:8,I) == MAXL(I));
   J        = find(abs(EL1(6:8,I) - MAXL(I)) < TOL);
   EL2(:,I) = [EL1(J,I); EL1(J+1,I); EL1(J+2,I)];
end
EL2 = test02(p,EL2);
EL1 = test02(p,EL1);
clf, hold on
X = p(1,:); Y = p(2,:); Z = zeros(1,length(X));
trimesh(t',X,Y,Z), hold on
EL3 = zeros(4,N);
for I = 1:N
   for J = (I+1):N
      if abs(EL1(1,I) - EL2(2,J)) < TOL & abs(EL1(2,I) - EL2(1,J)) < TOL
         EL3(:,I) = [EL1(1,I); EL2(3,J); EL1(2:3,I)];
         EL2(:,J) = 0;
      end
      if abs(EL1(2,I) - EL2(2,J)) < TOL & abs(EL1(3,I) - EL2(1,J)) < TOL
         EL3(:,I) = [EL1(1:2,I); EL2(3,J); EL1(3,I)];
         EL2(:,J) = 0;
      end
      if abs(EL1(3,I) - EL2(2,J)) < TOL & abs(EL1(1,I) - EL2(1,J)) < TOL
         EL3(:,I) = [EL1(1:3,I); EL2(3,J)];
         EL2(:,J) = 0;
      end
   end
end
K = find(EL3(1,:) ~= 0); q = EL3(:,K);
