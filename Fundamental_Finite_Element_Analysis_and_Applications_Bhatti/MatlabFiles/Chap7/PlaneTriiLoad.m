function rq = PlaneTriLoad(side, qn, qt, h, coord)
% PlaneTriLoad(side, qn, qt, h, coord)
% Generates equivalent load vector for a triangular element
% side = side over which the load is specified
% qn, qt = load components in the normal and the tangential direction
% h = thickness
% coord = coordinates at the element ends

x1=coord(1,1); y1=coord(1,2);
x2=coord(2,1); y2=coord(2,2);
x3=coord(3,1); y3=coord(3,2);
switch (side)
case 1
    L=sqrt((x2-x1)^2+(y2-y1)^2);
    nx=(y2-y1)/L; ny=-(x2-x1)/L;
    qx = nx*qn - ny*qt;
    qy = ny*qn + nx*qt;
    rq = h*L/2 * [qx; qy; qx; qy; 0; 0];
case 2
    L=sqrt((x2-x3)^2+(y2-y3)^2);
    nx=(y3-y2)/L; ny=-(x3-x2)/L;
    qx = nx*qn - ny*qt;
    qy = ny*qn + nx*qt;
    rq = h*L/2 * [0; 0; qx; qy; qx; qy];
case 3
    L=sqrt((x3-x1)^2+(y3-y1)^2);
    nx=(y1-y3)/L; ny=-(x1-x3)/L;
    qx = nx*qn - ny*qt;
    qy = ny*qn + nx*qt;
    rq = h*L/2 * [qx; qy; 0; 0; qx; qy];
end