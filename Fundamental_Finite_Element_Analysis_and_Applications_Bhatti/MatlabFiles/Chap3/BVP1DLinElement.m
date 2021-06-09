function [ke, re] = BVP1DLinElement(k, p, q, coord)
% [ke, re] = BVP1DLinElement(k, p, q, coord)
% Generates equations for a linear element for 1D BVP
% k,p,q = parameters defining the BVP
% coord = coordinates at the element ends

L=coord(2)-coord(1);
ke = [k/L - (L*p)/3, -(k/L) - (L*p)/6;
    -(k/L) - (L*p)/6, k/L - (L*p)/3];
re = [(L*q)/2; (L*q)/2];
