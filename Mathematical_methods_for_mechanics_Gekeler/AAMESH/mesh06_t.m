function [p1,e1,t1] = mesh06_t(p,e,t);
% Eckart Gekeler, Universitaet Stuttgart, Release 20.1.06
% Berechnung der Zwischenpunkte bei Dreiecken
% fuer gerade quadratische Dreieckselemente
% und gerade Randstuecke
% p1      : Zwischenpunkte
% t1(:,I) : Nrn der Zwischenpunkte in t(:,I);
% e1(6,I) : Nr. des Zwischenpunktes in e(:,I);
% e WIRD DURCH EINE ZEILE ZU e1 ERGAENZT!
% Grafik aus/ein (0/1)?
GRAFIK = 0;
M = size(p,2); N = size(e,2); L = size(t,2); MA = 0;
p1 = []; t1 = [];
e1 = [e;zeros(1,size(e,2))];
X = p(1,:); Y = p(2,:); Z1 = zeros(1,M);
if GRAFIK == 1
   clf, hold on, trimesh(t(1:3,:)',X,Y,Z1), hold on
   axis equal 
end
for I = 1:L
   M1 = MA+1; M2 = MA+2; M3 = MA+3;
   t1 = [t1,[M1; M2; M3]];
   J = t(1:3,I);
   ZWP =[(p(:,J(1))+p(:,J(2)))/2, ...
         (p(:,J(2))+p(:,J(3)))/2, ...
         (p(:,J(3))+p(:,J(1)))/2];
   p1 = [p1,ZWP];
   if GRAFIK == 1
      XA = p(1,J); YA = p(2,J);
      fill(XA,YA,'g'), hold on
      plot(ZWP(1,:),ZWP(2,:),'k*'), hold on
      pause(0.1)
   end
   MA = MA + 3;
end
% -- doppelte Zwischenknoten beseitigen ----
[M1,N1] = size(t1);
[p1,I,J] = unique(p1','rows');
p1 = p1'; C = t1(:); D = J(C);
t1 = reshape(D,M1,N1);
% -- Zwischenpunkte fuer Randstuecke -------
p_AUX   = (p(:,e(1,:)) + p(:,e(2,:)))/2;
[C,I,K] = intersect(p_AUX',p1','rows');
e1(6,I) = K;
t1      = t1 + M;
e1(6,:) = e1(6,:) + M;
