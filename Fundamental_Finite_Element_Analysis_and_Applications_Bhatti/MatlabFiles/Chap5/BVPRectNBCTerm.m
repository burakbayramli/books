function [ka, rb] = BVPRectNBCTerm(side, alpha, beta, coord)
% [ka, rb] = BVPRectNBCTerm(side, alpha, beta, coord)
% Generates kalpha and rbeta when NBC is specified along a side
% side = side over which the NBC is specified
% alpha and beta = coefficients specifying the NBC
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
x3=coord(3,1); y3=coord(3,2);
x4=coord(4,1); y4=coord(4,2);
ka = zeros(4); rb = zeros(4,1);
switch (side)
case 1
    lm=[1,2]; L=abs(x2-x1);
case 2
    lm=[2,3]; L=abs(y3-y2);
case 3
    lm=[3,4]; L=abs(x4-x3);
case 4
    lm=[4,1]; L=abs(y4-y1);
end
ka(lm, lm) = -(1/3)*alpha*L/2*[2, 1; 1, 2];
rb(lm) = beta*L/2*[1; 1];