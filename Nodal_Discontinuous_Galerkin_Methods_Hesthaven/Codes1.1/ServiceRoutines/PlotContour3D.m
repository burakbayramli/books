function PlotContour3D(Nout, u, levels)

% function PlotContour3D(Nout, u, levels)
% Purpose: generic routine to plot contours for triangulated data

Globals3D;

% create fine output grid
[ro,so,to] = EquiNodes3D(Nout);
Vo = Vandermonde3D(N, ro, so, to);
interp = Vo*invV;

xo = interp*x; yo = interp*y; zo = interp*z; uo = interp*u;
xo = xo(:); yo = yo(:); zo = zo(:); uo = uo(:);

% form symmetric tetrahedralization of local nodes
startrow = zeros(Nout+1, Nout+1);

sk = 1;
for i=0:Nout
  for j=0:Nout-i
    startrow(j+1, i+1) = sk;
    sk = sk + Nout+1-i-j;
  end
end

% contruct tetrahedralization
ltet = zeros(N*N*N,4);

sk = 1;
for i=0:Nout
  for j=0:Nout-i
    for k=0:Nout-i-j-1
      % Add Tet 1
      ltet(sk,1) = startrow(j+1, i+1)+k;
      ltet(sk,2) = startrow(j+1, i+1)+k+1;
      ltet(sk,3) = startrow(j+2, i+1)+k;
      ltet(sk,4) = startrow(j+1, i+2)+k;
      sk = sk+1;
      
      if(k < Nout-i-j-1)
	% Add Tet 2
        ltet(sk,1) = startrow(j+1, i+1)+k+1;
        ltet(sk,2) = startrow(j+2, i+1)+k;
        ltet(sk,3) = startrow(j+1, i+2)+k;
        ltet(sk,4) = startrow(j+1, i+2)+k+1;
        sk = sk+1;
	
        % Add Tet 3
        ltet(sk,1) = startrow(j+1, i+1)+k+1;
        ltet(sk,2) = startrow(j+2, i+1)+k+1;
        ltet(sk,3) = startrow(j+2, i+1)+k;
        ltet(sk,4) = startrow(j+1, i+2)+k+1;
        sk = sk+1;
	
        % Add Tet 4
        ltet(sk,1) = startrow(j+1, i+2)+k;
        ltet(sk,2) = startrow(j+2, i+2)+k;
        ltet(sk,3) = startrow(j+1, i+2)+k+1;
        ltet(sk,4) = startrow(j+2, i+1)+k;
	    sk = sk+1;
	
        % Add Tet 5
        ltet(sk,1) = startrow(j+2, i+1)+k;
        ltet(sk,2) = startrow(j+2, i+1)+k+1;
        ltet(sk,3) = startrow(j+2, i+2)+k;
        ltet(sk,4) = startrow(j+1, i+2)+k+1;
	    sk = sk+1;
      end
      
      if(k < Nout-i-j-2)
	% Add Tet 6
        ltet(sk,1) = startrow(j+1, i+2)+k+1;
        ltet(sk,2) = startrow(j+2, i+1)+k+1;
        ltet(sk,3) = startrow(j+2, i+2)+k;
        ltet(sk,4) = startrow(j+2, i+2)+k+1;
        sk = sk+1;
      end
    end
  end
end 

Nltet = sk-1;

% create global tet mesh
Npo = size(ro,1);
tet = zeros(K*Nltet, 4);
ids = 1:Nltet;
for k=1:K
  tet(ids, :) = ltet + (k-1)*Npo;
  ids = ids+Nltet;
end

Nlevels = length(levels);

minu = min(uo(tet), [], 2);
maxu = max(uo(tet), [], 2);

v1 = tet(:,1); v2 = tet(:,2); v3 = tet(:,3); v4 = tet(:,4); 
u1 = uo(v1);    u2 = uo(v2);    u3 = uo(v3);    u4 = uo(v4);    
x1 = xo(v1);    x2 = xo(v2);    x3 = xo(v3);    x4 = xo(v4);    
y1 = yo(v1);    y2 = yo(v2);    y3 = yo(v3);    y4 = yo(v4);    
z1 = zo(v1);    z2 = zo(v2);    z3 = zo(v3);    z4 = zo(v4);    

trix = []; triy = []; triz = []; triu = [];;

for n=1:Nlevels
  lev = levels(n);

  % find candidate elements
  ks = find(minu<= lev & maxu >= lev);
  
  Nks = length(ks);

  if(Nks>0)

    fc = zeros(Nks,6);

    u1ks = u1(ks); u2ks = u2(ks); u3ks = u3(ks); u4ks = u4(ks);

    % find local coordinate at each of 6 edges
    c1 = (lev-u1ks)./(u2ks-u1ks);  fc(:,1) = (min(u1ks,u2ks)<lev & max(u1ks,u2ks)>lev);
    c2 = (lev-u2ks)./(u3ks-u2ks);  fc(:,2) = (min(u2ks,u3ks)<lev & max(u2ks,u3ks)>lev);
    c3 = (lev-u3ks)./(u1ks-u3ks);  fc(:,3) = (min(u3ks,u1ks)<lev & max(u3ks,u1ks)>lev);
    c4 = (lev-u1ks)./(u4ks-u1ks);  fc(:,4) = (min(u1ks,u4ks)<lev & max(u1ks,u4ks)>lev);
    c5 = (lev-u2ks)./(u4ks-u2ks);  fc(:,5) = (min(u2ks,u4ks)<lev & max(u2ks,u4ks)>lev);
    c6 = (lev-u3ks)./(u4ks-u3ks);  fc(:,6) = (min(u3ks,u4ks)<lev & max(u3ks,u4ks)>lev);

    % find triangle intersection
    tris = find(sum(fc, 2)==3); Ntris = length(tris);  tfc = fc(tris,:); ids = find(tfc');
    ktris = ks(tris);

    % trim list
    tc1 = c1(tris); tc2 = c2(tris); tc3 = c3(tris); tc4 = c4(tris); tc5 = c5(tris); tc6 = c6(tris);

    tx1 = x1(ktris);   ty1 = y1(ktris);   tz1 = z1(ktris);
    tx2 = x2(ktris);   ty2 = y2(ktris);   tz2 = z2(ktris);
    tx3 = x3(ktris);   ty3 = y3(ktris);   tz3 = z3(ktris);
    tx4 = x4(ktris);   ty4 = y4(ktris);   tz4 = z4(ktris);

    xc = zeros(Ntris, 6); yc = zeros(Ntris, 6); zc = zeros(Ntris, 6);
    xc(:,1) = (1-tc1).*tx1 + tc1.*tx2; yc(:,1) = (1-tc1).*ty1 + tc1.*ty2; zc(:,1) = (1-tc1).*tz1 + tc1.*tz2;
    xc(:,2) = (1-tc2).*tx2 + tc2.*tx3; yc(:,2) = (1-tc2).*ty2 + tc2.*ty3; zc(:,2) = (1-tc2).*tz2 + tc2.*tz3;
    xc(:,3) = (1-tc3).*tx3 + tc3.*tx1; yc(:,3) = (1-tc3).*ty3 + tc3.*ty1; zc(:,3) = (1-tc3).*tz3 + tc3.*tz1;
    xc(:,4) = (1-tc4).*tx1 + tc4.*tx4; yc(:,4) = (1-tc4).*ty1 + tc4.*ty4; zc(:,4) = (1-tc4).*tz1 + tc4.*tz4;
    xc(:,5) = (1-tc5).*tx2 + tc5.*tx4; yc(:,5) = (1-tc5).*ty2 + tc5.*ty4; zc(:,5) = (1-tc5).*tz2 + tc5.*tz4;
    xc(:,6) = (1-tc6).*tx3 + tc6.*tx4; yc(:,6) = (1-tc6).*ty3 + tc6.*ty4; zc(:,6) = (1-tc6).*tz3 + tc6.*tz4;
    xc = xc'; yc = yc'; zc = zc';

    ids = reshape(ids, 3, Ntris);

    trix = [trix, xc(ids)];
    triy = [triy, yc(ids)];
    triz = [triz, zc(ids)];
    triu = [triu, lev*ones(size(ids))];

    % find quadrilateral intersection
    quads = find(sum(fc, 2)==4); Nquads = length(quads);  qfc = fc(quads,:); ids = find(qfc');
    kquads = ks(quads);

    % quad list
    qc1 = c1(quads); qc2 = c2(quads); qc3 = c3(quads); qc4 = c4(quads); qc5 = c5(quads); qc6 = c6(quads);

    qx1 = x1(kquads);   qy1 = y1(kquads);   qz1 = z1(kquads);
    qx2 = x2(kquads);   qy2 = y2(kquads);   qz2 = z2(kquads);
    qx3 = x3(kquads);   qy3 = y3(kquads);   qz3 = z3(kquads);
    qx4 = x4(kquads);   qy4 = y4(kquads);   qz4 = z4(kquads);

    xc = zeros(Nquads, 6); yc = zeros(Nquads, 6); zc = zeros(Nquads, 6);
    xc(:,1) = (1-qc1).*qx1 + qc1.*qx2; yc(:,1) = (1-qc1).*qy1 + qc1.*qy2; zc(:,1) = (1-qc1).*qz1 + qc1.*qz2;
    xc(:,2) = (1-qc2).*qx2 + qc2.*qx3; yc(:,2) = (1-qc2).*qy2 + qc2.*qy3; zc(:,2) = (1-qc2).*qz2 + qc2.*qz3;
    xc(:,3) = (1-qc3).*qx3 + qc3.*qx1; yc(:,3) = (1-qc3).*qy3 + qc3.*qy1; zc(:,3) = (1-qc3).*qz3 + qc3.*qz1;
    xc(:,4) = (1-qc4).*qx1 + qc4.*qx4; yc(:,4) = (1-qc4).*qy1 + qc4.*qy4; zc(:,4) = (1-qc4).*qz1 + qc4.*qz4;
    xc(:,5) = (1-qc5).*qx2 + qc5.*qx4; yc(:,5) = (1-qc5).*qy2 + qc5.*qy4; zc(:,5) = (1-qc5).*qz2 + qc5.*qz4;
    xc(:,6) = (1-qc6).*qx3 + qc6.*qx4; yc(:,6) = (1-qc6).*qy3 + qc6.*qy4; zc(:,6) = (1-qc6).*qz3 + qc6.*qz4;
    xc = xc'; yc = yc'; zc = zc';

    ids1 = reshape(ids, 4, Nquads);
    ids2 = ids1([1;4;2;3],:);
    ids3 = ids1([1;3;4;2],:);

    % find length perimeters bounded by 4 nodes
    perims1 = QuadPerimeter3D(xc(ids1),yc(ids1),zc(ids1));
    perims2 = QuadPerimeter3D(xc(ids2),yc(ids2),zc(ids2));    
    perims3 = QuadPerimeter3D(xc(ids3),yc(ids3),zc(ids3));

    % locate minimum perimeter quad
    idsA = find(perims1<min(perims2, perims3));
    idsB = find(perims2<min(perims1, perims3));    
    idsC = find(perims3<min(perims1, perims2));

    % use only minimum perimeter quads
    ids  = [ids1(:,idsA),ids2(:,idsB),ids3(:,idsC)];

    % divide into two triangles (ok since quad must be convex)
    ids1 = [ids([1;2;3],:),ids([1;3;4],:)];

    % add triangles to list
    trix = [trix, xc(ids1)];
    triy = [triy, yc(ids1)];
    triz = [triz, zc(ids1)];
    triu = [triu, lev*ones(size(ids1))];

  end 
end

ha = patch(trix,triy,triz,triu); shading interp
set(ha, 'EdgeColor', 'none')

camlight; camlight; material shiny; lighting gouraud; view(3); axis equal; 
axis off; 

return;
