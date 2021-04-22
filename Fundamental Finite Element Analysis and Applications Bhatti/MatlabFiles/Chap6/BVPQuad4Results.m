function results = BVPQuad4Results(coord, dn)
% results = BVPQuad4Results(coord, dn)
% Computes element solution for a quadrilateral element for 2D BVP
% coord = nodal coordinates
% dn = nodal solution
% The solution is computed at the element center
% The output variables are loc, u and its x and y derivatives
s = 0; t = 0;
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
results=[x, y, n*dn, bx*dn, by*dn];