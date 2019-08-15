function bld090502
% Singulaere Elemente
clf, clc, clear
s =2/3;
THETA = linspace(0,pi/s,30);
RHO = linspace(0,0.1,30);
[th,r] = meshgrid(THETA,RHO);
[X,Y] = pol2cart(th,r);
%Z = r.^s.*sin(s*th);
Z = s*r.^(s-1).*sin(s*th);
mesh(X,Y,Z);
view(150,30)
axis tight, axis off
