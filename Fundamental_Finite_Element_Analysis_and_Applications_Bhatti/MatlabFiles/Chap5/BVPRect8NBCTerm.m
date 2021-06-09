function [ka, rb] = BVPRect8NBCTerm(side, alpha, beta, coord)
% [ka, rb] = BVPRect8NBCTerm(side, alpha, beta, coord)
% 8 node rectangular element for 2d BVP
% Generates kalpha and rbeta when NBC is specified along a side
% side = side over which the NBC is specified
% alpha and beta = coefficients specifying the NBC
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2);
x3=coord(3,1); y7=coord(7,2);
a = abs(x3 - x1)/2; b = abs(y7 - y1)/2;
switch (side)
case 1
    ka = [-((4*a*alpha)/15), -((2*a*alpha)/15),... 
            (a*alpha)/15, 0, 0, 0, 0, 0;
        -((2*a*alpha)/15), -((16*a*alpha)/15),... 
            -((2*a*alpha)/15), 0, 0, 0, 0, 0;
        (a*alpha)/15, -((2*a*alpha)/15), ...
            -((4*a*alpha)/15), 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0];
    rb = [(a*beta)/3; (4*a*beta)/3; (a*beta)/3;
        0; 0; 0; 0; 0];
case 2
    ka = [0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, -((4*alpha*b)/15),... 
            -((2*alpha*b)/15), (alpha*b)/15, 0, 0, 0;
        0, 0, -((2*alpha*b)/15), -((16*alpha*b)/15), ...
            -((2*alpha*b)/15), 0, 0, 0;
        0, 0, (alpha*b)/15, -((2*alpha*b)/15),... 
            -((4*alpha*b)/15), 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0];
    rb = [0; 0; (b*beta)/3; (4*b*beta)/3; (b*beta)/3; 0; 
        0; 0];
case 3
    ka = [0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 
        -((4*a*alpha)/15), -((2*a*alpha)/15),... 
            (a*alpha)/15, 0;
        0, 0, 0, 0,-((2*a*alpha)/15), -((16*a*alpha)/15), ...
            -((2*a*alpha)/15), 0;
        0, 0, 0, 0, (a*alpha)/15, -((2*a*alpha)/15),... 
            -((4*a*alpha)/15), 0;
        0, 0, 0, 0, 0, 0, 0, 0];
    rb = [0; 0; 0; 0; (a*beta)/3; (4*a*beta)/3;
        (a*beta)/3; 0];
case 4
    ka = [-((4*alpha*b)/15), 0, 0, 0, 0, 0, (alpha*b)/15,... 
            -((2*alpha*b)/15);
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        0, 0, 0, 0, 0, 0, 0, 0;
        (alpha*b)/15, 0, 0, 0, 0, 0,...
            -((4*alpha*b)/15), -((2*alpha*b)/15);
        -((2*alpha*b)/15), 0, 0, 0, 0, 0,...
            -((2*alpha*b)/15), -((16*alpha*b)/15)];
    rb = [(b*beta)/3; 0; 0; 0; 0; 0; (b*beta)/3;(4*b*beta)/3];
end