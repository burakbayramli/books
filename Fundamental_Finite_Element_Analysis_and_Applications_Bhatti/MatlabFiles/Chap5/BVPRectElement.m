function [ke, rq] = BVPRectElement(kx, ky, p, q, coord)
% [ke, rq] = BVPRectElement(kx, ky, p, q, coord)
% Generates for a rectangular element for 2d BVP
% kx, ky, p, q = parameters defining the BVP
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
x3=coord(3,1); y3=coord(3,2);
x4=coord(4,1); y4=coord(4,2);
a = abs((x2 - x1))/2; b = abs((y4 - y2))/2;
kk = (kx*b)/(6*a)*[2, -2, -1, 1; -2, 2, 1, -1;
    -1, 1, 2, -2; 1, -1, -2, 2] + ...
    (ky*a)/(6*b)*[2, 1, -1, -2; 1, 2, -2, -1;
    -1, -2, 2, 1; -2, -1,1, 2];
kp = -((p*a*b)/9)*[4, 2,1, 2; 2, 4, 2, 1; 1, 2, 4, 2; 2, 1, 2, 4];
ke = kk + kp; 
rq = a*b*q*[1; 1; 1; 1]; 
