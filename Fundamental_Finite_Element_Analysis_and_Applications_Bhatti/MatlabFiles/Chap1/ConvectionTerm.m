function [kh, rh] = ConvectionTerm(side, h, Tinf, coord)
% [kh, rh] = ConvectionTerm(side, h, Tinf, coord)
% Generates kh and rh when convection is specified for a triangular element
% side = side over which the convection is specified
% h = convection coefficient
% Tinf = surrounding temperature
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
x3=coord(3,1); y3=coord(3,2);
switch (side)
case 1
    L=sqrt((x2-x1)^2+(y2-y1)^2);
    kh=h*L/6 * [2,1,0; 1,2,0; 0,0,0];
    rh=h*Tinf *L/2 * [1; 1; 0];
case 2
    L=sqrt((x2-x3)^2+(y2-y3)^2);
    kh=h*L/6 * [0,0,0; 0,2,1; 0,1,2];
    rh=h*Tinf *L/2 * [0; 1; 1];
case 3
    L=sqrt((x3-x1)^2+(y3-y1)^2);
    kh=h*L/6 * [2,0,1; 0,0,0; 1,0,2];
    rh=h*Tinf *L/2 * [1; 0; 1];
end