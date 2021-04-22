function [ke, re] = BVP1DQuadElement(k, p, q, coord)
% [ke, re] = BVP1DQuadElement(k, p, q, coord)
% Generates equations for a quadratic element for 1D BVP
% k,p,q = parameters defining the BVP
% coord = coordinates at the element ends

L=coord(3)-coord(1);
ke = [(7*k)/(3*L) - (2*L*p)/15, (-8*k)/(3*L) - (L*p)/15, ...
        k/(3*L) + (L*p)/30;
    (-8*k)/(3*L) - (L*p)/15, (16*k)/(3*L) - (8*L*p)/15, ...
        (-8*k)/(3*L) - (L*p)/15;
    k/(3*L) + (L*p)/30, (-8*k)/(3*L) - (L*p)/15, ...
        (7*k)/(3*L) - (2*L*p)/15];
re = [(L*q)/6; (2*L*q)/3; (L*q)/6];