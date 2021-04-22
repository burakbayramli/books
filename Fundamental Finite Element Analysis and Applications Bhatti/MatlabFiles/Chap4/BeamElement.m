function [ke, rq] = BeamElement(EI, q, coord)
% [ke, rq] = BeamElement(EI, q, coord)
% Generates equations for a beam element
% EI = beam stiffness
% q = distributed load
% coord = coordinates at the element ends

L=coord(2)-coord(1);
ke = [(12*EI)/L^3, (6*EI)/L^2, -((12*EI)/L^3), (6*EI)/L^2;
    (6*EI)/L^2, (4*EI)/L, -((6*EI)/L^2), (2*EI)/L;
    -((12*EI)/L^3), -((6*EI)/L^2), (12*EI)/L^3, -((6*EI)/L^2);
    (6*EI)/L^2, (2*EI)/L, -((6*EI)/L^2), (4*EI)/L];
rq = [(L*q)/2; (L^2*q)/12; (L*q)/2; -((L^2*q)/12)];