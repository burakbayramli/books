function [ke, rq] = BVPQuad4Element(kx, ky, p, q, coord)
% [ke, rq] = BVPQuad4Element(kx, ky, p, q, coord)
% Generates for a 4 node quadrilateral element for 2d BVP
% kx, ky, p, q = parameters defining the BVP
% coord = coordinates at the element ends

% Use 2x2 integration. Gauss point locations and weights
pt=1/sqrt(3);
gpLocs = [-pt,-pt; -pt,pt; pt,-pt; pt,pt];
gpWts = [1,1,1,1];
kk=zeros(4); kp=zeros(4); rq=zeros(4,1);
for i=1:length(gpWts)
    s = gpLocs(i, 1); t = gpLocs(i, 2); w = gpWts(i);
    n = [(1/4)*(1 - s)*(1 - t), (1/4)*(s + 1)*(1 -t), ...
            (1/4)*(s + 1)*(t + 1), (1/4)*(1 - s)*(t + 1)];
    dns=[(-1 + t)/4, (1 - t)/4, (1 + t)/4, (-1 - t)/4];
    dnt=[(-1 + s)/4, (-1 - s)/4, (1 + s)/4, (1 - s)/4];
    x = n*coord(:,1); y = n*coord(:,2);
    dxs = dns*coord(:,1); dxt = dnt*coord(:,1);
    dys = dns*coord(:,2); dyt = dnt*coord(:,2);
    J = [dxs, dxt; dys, dyt]; detJ = det(J);
    bx = (J(2, 2)*dns - J(2, 1)*dnt)/detJ;
    by = (-J(1, 2)*dns + J(1, 1)*dnt)/detJ;
    b = [bx; by];
    c = [kx, 0; 0, ky];
    kk = kk + detJ*w* b'*c*b;
    kp = kp - detJ*w*p * n'*n;
    rq = rq + detJ*w*q * n';
end
ke=kk+kp;
