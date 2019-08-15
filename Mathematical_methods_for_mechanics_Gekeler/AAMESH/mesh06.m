function [p1,e1,t1] = mesh06(p,e,t);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Berechnung der Zwischenpunkte bei Dreiecken
% fuer gerade quadratische Dreieckselemente
% p1      : Zwischenpunkte
% t1(:,I) : Nrn der Zwischenpunkte in EL(:,I);
% e1(I)   : Nr. des Zwischenpunktes in e(I);

M = size(p,2); N = size(e,2); L = size(t,2);
p1 = []; e1 = zeros(2,N); t1 = zeros(3,L);
X = p(1,:); Y = p(2,:); Z1 = zeros(1,M);
clf, hold on
%trimesh(t(1:3,:)',X,Y,Z1), hold on
MA = 0;
for I = 1:L
   M1 = MA+1; M2 = MA+2; M3 = MA+3;
   t1(:,I) = [M1; M2; M3];
   J   = zeros(3,1);
   IND = t(1:3,I);
   %XA = p(1,IND); YA = p(2,IND);
   %fill(XA,YA,'g'), hold on
   for P = 1:3
      J(P) = find([1:M] == IND(P));
   end
   AAA =[(p(:,J(1))+p(:,J(2)))/2, ...
            (p(:,J(2))+p(:,J(3)))/2, ...
         (p(:,J(3))+p(:,J(1)))/2];
   p1 = [p1,AAA];
   %plot(AAA(1,:),AAA(2,:),'k*'), hold on
   MA   = MA + 3;
 %  pause
end
[p1,t1] = mesh04(p1,t1);
for I = 1:N
   p_AUX = (p(:,e(1,I)) + p(:,e(2,I)))/2;
   for K = 1:size(p1,2)
       if p1(:,K) == p_AUX
          e1(:,I) = [K; e(5,I)];
       end
    end
end
t1 = t1 + M;
e1(1,:) = e1(1,:) + M;
