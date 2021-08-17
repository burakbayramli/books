function PlotAdaptiveContour3D(u, levels, tol)

% function PlotAdaptiveContour3D(u, levels, tol)
% Purpose: adaptively refine the mesh to approximately locate isocontours

Globals3D;

% build interpolation matrix (coarse->fine)
EToVi = [1 5 7 8; 5 2 6 9; 7 6 3 10; 8 9 10 4; 8 5 7 9; 7 5 6 9; 8 9 7 10; 9 6 7 10];
VXi   = [-1  1 -1 -1  0  0 -1 -1  0 -1];
VYi   = [-1 -1  1 -1 -1  0  0 -1 -1  0];
VZi   = [-1 -1 -1  1 -1 -1 -1  0  0  0];

v1 = EToVi(:,1); v2 = EToVi(:,2); v3 = EToVi(:,3); v4 = EToVi(:,4);
ri = 0.5*(-(r+s+t+1)*VXi(v1) + (1+r)*VXi(v2) + (1+s)*VXi(v3) + (1+t)*VXi(v4) );
si = 0.5*(-(r+s+t+1)*VYi(v1) + (1+r)*VYi(v2) + (1+s)*VYi(v3) + (1+t)*VYi(v4) );
ti = 0.5*(-(r+s+t+1)*VZi(v1) + (1+r)*VZi(v2) + (1+s)*VZi(v3) + (1+t)*VZi(v4) );

interp = Vandermonde3D(N, ri(:), si(:), ti(:))*invV;
ri = [-1;1;-1;-1]; si = [-1;-1;1;-1]; ti = [-1;-1;-1;1]; refNp = length(ri);
interp1 = Vandermonde3D(N, ri(:), si(:), ti(:))*invV;

Nlevels = length(levels);

sk = 1;
F = spalloc(Np,Np,1);
for i=0:N % old ordering
  for j=0:N - i
    for k=0:N - i - j
      if(i+j+k<=1), F(sk,sk) = 1.; end;
      sk = sk+1;
    end
  end
end

hold on;
for n=1:Nlevels
  
  xref = x; yref = y; zref = z; uref = u; Kref = K; Jref = J;
  
  err = 1;
  while(err > tol)
    
    level = levels(n);
    
    umin = min(uref, [], 1);
    umax = max(uref, [], 1);
    
    refineflag = (umin <= level & umax >= level);
    
    toref = find( refineflag);
    Nref = length(toref);
    
    uref = reshape(interp*uref(:,toref), Np, 8*Nref);
    xref = reshape(interp*xref(:,toref), Np, 8*Nref);
    yref = reshape(interp*yref(:,toref), Np, 8*Nref);
    zref = reshape(interp*zref(:,toref), Np, 8*Nref);
    Jref = reshape(interp*Jref(:,toref), Np, 8*Nref)/8;

    Kref = 8*Nref;

    ufilt = V*(F*(invV*uref));
    err = max(max(abs(ufilt-uref)));

  end
  
  xref = interp1*xref; yref = interp1*yref; zref = interp1*zref; uref = interp1*uref; 
  
  tets = reshape(1:4*Kref, 4, Kref)';
  
  x1 = xref(1,:)'; y1 = yref(1,:)'; z1 = zref(1,:)'; u1 = uref(1,:)' + 1e-10*rand(Kref,1);
  x2 = xref(2,:)'; y2 = yref(2,:)'; z2 = zref(2,:)'; u2 = uref(2,:)' + 1e-10*rand(Kref,1);
  x3 = xref(3,:)'; y3 = yref(3,:)'; z3 = zref(3,:)'; u3 = uref(3,:)' + 1e-10*rand(Kref,1);
  x4 = xref(4,:)'; y4 = yref(4,:)'; z4 = zref(4,:)'; u4 = uref(4,:)' + 1e-10*rand(Kref,1);
  
  trix = []; triy = []; triz = []; triu = [];;
  
  umin = min(uref, [], 1);
  umax = max(uref, [], 1);
  
  lev = levels(n);
  
  % find candidate elements
  ks = find(umin<= lev & umax >= lev);
  
  Nks = length(ks);
  
  if(Nks>0)
    
    fc = zeros(Nks,6);
    
    u1ks = u1(ks); u2ks = u2(ks); u3ks = u3(ks); u4ks = u4(ks);
    
    % find local coordinate at each of 6 edges
    c1 = (lev-u1ks)./(u2ks-u1ks);  fc(:,1) = (c1>=0 & c1<=1);
    c2 = (lev-u2ks)./(u3ks-u2ks);  fc(:,2) = (c2>=0 & c2<=1);
    c3 = (lev-u3ks)./(u1ks-u3ks);  fc(:,3) = (c3>=0 & c3<=1);
    c4 = (lev-u1ks)./(u4ks-u1ks);  fc(:,4) = (c4>=0 & c4<=1);    
    c5 = (lev-u2ks)./(u4ks-u2ks);  fc(:,5) = (c5>=0 & c5<=1);    
    c6 = (lev-u3ks)./(u4ks-u3ks);  fc(:,6) = (c6>=0 & c6<=1);    

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

  size(trix)
  
  ha = patch(trix,triy,triz,triu); shading interp; 
  
  set(ha, 'EdgeColor', 'none')
end
hold off;

camlight; material shiny; lighting gouraud; view(3); axis equal; 
axis off; 

return;

