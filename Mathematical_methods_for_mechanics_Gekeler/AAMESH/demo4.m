function demo4
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
clear,clc
disp(' Halbierung der laengsten Kanten, Bildfolge ')
[p,e,t] = bsp001b;
disp(' Refinemesh ')
%[p,e,t] = mesh01(p,e,t);
disp(' Jigglemesh ')
%p = mesh10(p,e,t,4);
bild00(p,e,t);
pause
[p,t] = mesh17(p,t);
bild00(p,e,t);
pause
p = mesh10(p,e,t,4);
bild00(p,e,t);
