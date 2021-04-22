function [ka, rb] = BVPTriNBCTerm(side, alpha, beta, coord)
% [ka, rb] = BVPTriNBCTerm(side, alpha, beta, coord)
% Generates kalpha and rbeta when NBC is specified along a side
% side = side over which the NBC is specified
% alpha and beta = coefficients specifying the NBC
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
x3=coord(3,1); y3=coord(3,2);
switch (side)
case 1
    L = sqrt((x2-x1)^2+(y2-y1)^2);
    ka = -alpha*L/6 * [2,1,0; 1,2,0; 0,0,0];
    rb = beta *L/2 * [1; 1; 0];
case 2
    L = sqrt((x2-x3)^2+(y2-y3)^2);
    ka = -alpha*L/6 * [0,0,0; 0,2,1; 0,1,2];
    rb = beta *L/2 * [0; 1; 1];
case 3
    L = sqrt((x3-x1)^2+(y3-y1)^2);
    ka = -alpha*L/6 * [2,0,1; 0,0,0; 1,0,2];
    rb = beta *L/2 * [1; 0; 1];
end