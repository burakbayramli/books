function rq = PlaneQuad4Load(side, qn, qt, h, coord)
% rq = PlaneQuad4Load(side, qn, qt, h, coord)
% Generates equivalent load vector for a triangular element
% side = side over which the load is specified
% qn, qt = load components in the normal and the tangential direction
% h = thickness
% coord = coordinates at the element ends

% Use 2 point integration. Gauss point locations and weights
pt=-1/sqrt(3);
gpLocs = [-pt, pt];
gpWts = [1,1];
rq=zeros(8,1);
for i=1:length(gpWts)
    a = gpLocs(i); w = gpWts(i);
    switch (side)
    case 1
        n = [(1 - a)/2, (1 + a)/2, 0, 0];
        dna = [-1/2, 1/2, 0, 0];
    case 2
        n = [0, (1 - a)/2, (1 + a)/2, 0];
        dna = [0, -1/2, 1/2, 0];
    case 3
        n = [0, 0, (1 - a)/2, (1 + a)/2];
        dna = [0, 0, -1/2, 1/2];
    case 4
        n = [(1 + a)/2, 0, 0, (1 - a)/2];
        dna = [1/2, 0, 0, -1/2];
    end
    dxa = dna*coord(:,1); dya = dna*coord(:,2);
    Jc=sqrt(dxa^2 + dya^2);
    nx = dya/Jc; ny = -dxa/Jc;
    qx = nx*qn - ny*qt;
    qy = ny*qn + nx*qt;
    n = [n(1),0,n(2),0,n(3),0,n(4),0;
        0,n(1),0,n(2),0,n(3),0,n(4)];
    rq = rq + h*Jc*w*n'*[qx; qy];
end
