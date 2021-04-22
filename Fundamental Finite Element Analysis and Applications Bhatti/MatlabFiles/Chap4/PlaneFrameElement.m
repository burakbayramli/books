function [ke, rq] = PlaneFrameElement(modulus, inertia, A, qs, qt, coord)
% [ke, rq] = PlaneFrameElement(modulus, inertia, A, qs, qt, coord)
% Generates equations for a plane frame element
% modulus = modulus of elasticity
% inertia = moment of inertia
% A = area of cross-section
% qs = distributed load along the element axis
% qt = distributed load normal to the element axis
% coord = coordinates at the element ends

EI=modulus*inertia; EA = modulus*A;
x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
L=sqrt((x2-x1)^2+(y2-y1)^2);
ls=(x2-x1)/L; ms=(y2-y1)/L;

ke = [(EA*L^2*ls^2 + 12*EI*ms^2)/L^3, ((-12*EI + EA*L^2)*ls*ms)/L^3, ...
        (-6*EI*ms)/L^2, -((EA*L^2*ls^2 + 12*EI*ms^2)/L^3), ...
        ((12*EI - EA*L^2)*ls*ms)/L^3, (-6*EI*ms)/L^2;
    ((-12*EI + EA*L^2)*ls*ms)/L^3, (12*EI*ls^2 + EA*L^2*ms^2)/L^3, ...
        (6*EI*ls)/L^2, ((12*EI - EA*L^2)*ls*ms)/L^3, ...
        -((12*EI*ls^2 + EA*L^2*ms^2)/L^3), (6*EI*ls)/L^2;
    (-6*EI*ms)/L^2, (6*EI*ls)/L^2, (4*EI)/L, ...
        (6*EI*ms)/L^2, (-6*EI*ls)/L^2,(2*EI)/L;
    -((EA*L^2*ls^2 + 12*EI*ms^2)/L^3), ((12*EI -EA*L^2)*ls*ms)/L^3, ...
        (6*EI*ms)/L^2, (EA*L^2*ls^2 + 12*EI*ms^2)/L^3, ...
        ((-12*EI + EA*L^2)*ls*ms)/L^3, (6*EI*ms)/L^2;
    ((12*EI - EA*L^2)*ls*ms)/L^3, -((12*EI*ls^2 + EA*L^2*ms^2)/L^3),...
        (-6*EI*ls)/L^2, ((-12*EI + EA*L^2)*ls*ms)/L^3, ...
        (12*EI*ls^2 + EA*L^2*ms^2)/L^3, (-6*EI*ls)/L^2;
    (-6*EI*ms)/L^2, (6*EI*ls)/L^2, (2*EI)/L, (6*EI*ms)/L^2, ...
        (-6*EI*ls)/L^2, (4*EI)/L];
rq = [(L*(ls*qs - ms*qt))/2; (L*(ms*qs + ls*qt))/2;(L^2*qt)/12; 
    (L*(ls*qs - ms*qt))/2; (L*(ms*qs + ls*qt))/2; -(L^2*qt)/12];
