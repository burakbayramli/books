function rq = HeatFluxTerm(side, q, coord)
% rq = HeatFluxTerm(side, q, coord)
% Generates vector resulting from a specified flux for a triangular element
% side = side over which the flux is specified
% q = flux value
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
x3=coord(3,1); y3=coord(3,2);
switch (side)
case 1
    rq = q*sqrt((x2 - x1)^2 + (y2 - y1)^2)/2 * [1; 1; 0];
case 2
    rq = q*sqrt((x2 - x3)^2 + (y2 - y3)^2)/2 * [0; 1; 1];
case 3
    rq = q*sqrt((x3 - x1)^2 + (y3 - y1)^2)/2 * [1; 0; 1];
end
