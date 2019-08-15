function demo8
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
disp(' Randdreiecke und deren Knoten ')
clear,clc, clf, hold on
[p,e,t,segnr1,segnr2] = bsp06;
disp(' Refinemesh ')
[p,e,t] = mesh01(p,e,t);
disp(' Jigglemesh ')
p = mesh10(p,e,t);
DIST = 0.5;
bild01(p,e,t,segnr1,segnr2,DIST)
disp(' Weiter mit beliebiger Taste ')
pause
[RDKN,t_rand,t_innen] = mesh23(p,e,t,segnr1);
plot(p(1,RDKN),p(2,RDKN),'k*'), hold on
for I = 1:size(t_rand,2)
   A = p(1,t_rand(:,I)); B = p(2,t_rand(:,I));
   fill(A,B,'y'), hold on
end

