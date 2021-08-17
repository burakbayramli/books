function PlotSlice3D(Nout, u, ax, axloc)

% function PlotSlice3D(Nout, u, ax, axloc)
% Purpose: generic routine to plot contours for triangulated data

Globals3D;

v1 = EToV(:,1); v2 = EToV(:,2); v3 = EToV(:,3); v4 = EToV(:,4); 
x1 = VX(v1)';   x2 = VX(v2)';   x3 = VX(v3)';   x4 = VX(v4)';    
y1 = VY(v1)';   y2 = VY(v2)';   y3 = VY(v3)';   y4 = VY(v4)';    
z1 = VZ(v1)';   z2 = VZ(v2)';   z3 = VZ(v3)';   z4 = VZ(v4)';    

xmin = min(VX(EToV), [], 2); xmax = max(VX(EToV), [], 2);
ymin = min(VY(EToV), [], 2); ymax = max(VY(EToV), [], 2);
zmin = min(VZ(EToV), [], 2); zmax = max(VZ(EToV), [], 2);

% find candidate elements
if(ax=='x'), ks = find(xmin<= axloc & xmax >= axloc); coord = VX(EToV); end;
if(ax=='y'), ks = find(ymin<= axloc & ymax >= axloc); coord = VY(EToV); end;
if(ax=='z'), ks = find(zmin<= axloc & zmax >= axloc); coord = VZ(EToV); end;

Nks = length(ks);

trix = []; triy = []; triz = []; trik = [];

if(Nks>0)

  fc = zeros(Nks,6);
  
  coord1ks = coord(ks,1); coord2ks = coord(ks,2); coord3ks = coord(ks,3); coord4ks = coord(ks,4);
  
  % find local coordinate at each of 6 edges
  c1 = (axloc-coord1ks)./(coord2ks-coord1ks);  fc(:,1) = (c1>=0 & c1<=1);
  c2 = (axloc-coord2ks)./(coord3ks-coord2ks);  fc(:,2) = (c2>=0 & c2<=1);
  c3 = (axloc-coord3ks)./(coord1ks-coord3ks);  fc(:,3) = (c3>=0 & c3<=1);
  c4 = (axloc-coord1ks)./(coord4ks-coord1ks);  fc(:,4) = (c4>=0 & c4<=1);    
  c5 = (axloc-coord2ks)./(coord4ks-coord2ks);  fc(:,5) = (c5>=0 & c5<=1);    
  c6 = (axloc-coord3ks)./(coord4ks-coord3ks);  fc(:,6) = (c6>=0 & c6<=1);    
  
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
  trik = [trik; ktris];

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
  flag1 = ones(4,1)*(perims1<min(perims2, perims3));
  flag2 = ones(4,1)*(perims2<min(perims1, perims3));
  flag3 = ones(4,1)*(perims3<min(perims1, perims2));

  ids = ids1.*flag1 + ids2.*flag2 + ids3.*flag3;

  % divide into two triangles (ok since quad must be convex)
  ids1 = [ids([1;2;3],:),ids([1;3;4],:)];
  
  % add triangles to list
  trix = [trix, xc(ids1)];
  triy = [triy, yc(ids1)];
  triz = [triz, zc(ids1)];
  trik = [trik; kquads; kquads]; 

  % now for each triangle interpolate to output nodes
  Ntrik = length(trik(:));

  % compute nodes for sections
  [req,seq] = EquiNodes2D(Nout); 
  ltris = delaunay(req,seq);
  Nltris = size(ltris,1);
  [req,seq] = xytors(req,seq);
  Neq = length(req);

  sampx = 0.5*( -(req+seq)*trix(1,:) + (1+req)*trix(2,:) + (1+seq)*trix(3,:));
  sampy = 0.5*( -(req+seq)*triy(1,:) + (1+req)*triy(2,:) + (1+seq)*triy(3,:));
  sampz = 0.5*( -(req+seq)*triz(1,:) + (1+req)*triz(2,:) + (1+seq)*triz(3,:));


  % vectorized routine to find weights for interpolation
  [sampw, samptet] = Sample3D(sampx(:), sampy(:), sampz(:));

  % compute interpolation
  Nsamps = size(sampw,1);
  sampu = zeros(size(sampx));
  for n=1:Nsamps
    k = samptet(n);
    if(~isnan(k))
      sampu(n) = sampw(n,:)*u(:,k);
    end
  end

  % form triangulation of sample nodes
  tris  = zeros(Nltris*Ntrik,3);  
  for n=1:Ntrik
    tris((n-1)*Nltris+1:n*Nltris,:) = ltris + (n-1)*Neq;
  end

  % plot slice
  trisurf(tris, sampx, sampy, sampz, sampu); shading interp; 
  
end
return
