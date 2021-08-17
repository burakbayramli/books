function PlotAdaptiveContour2D(u, levels, tol)

% function PlotAdaptiveContour2D(u, levels, tol)
% Purpose: adaptively refine the mesh to approximately locate isocontours

Globals2D;

% build interpolation matrix (coarse->fine)
ri(:,1) = 0.5*(-(r+s)*( 0) + (1+r)*( 0) + (1+s)*(-1) );
si(:,1) = 0.5*(-(r+s)*(-1) + (1+r)*( 0) + (1+s)*( 0) );

ri(:,2) = 0.5*(-(r+s)*(-1) + (1+r)*( 0) + (1+s)*(-1) );
si(:,2) = 0.5*(-(r+s)*(-1) + (1+r)*(-1) + (1+s)*( 0) );

ri(:,3) = 0.5*(-(r+s)*( 1) + (1+r)*( 0) + (1+s)*( 0) );
si(:,3) = 0.5*(-(r+s)*(-1) + (1+r)*( 0) + (1+s)*(-1) );

ri(:,4) = 0.5*(-(r+s)*(-1) + (1+r)*(-1) + (1+s)*( 0) );
si(:,4) = 0.5*(-(r+s)*( 1) + (1+r)*( 0) + (1+s)*( 0) );

%interp = Vandermonde2D(N, ri(:), si(:))*invV; 
interp = InterpMatrix2D(ri(:),si(:));

Nlevels = length(levels);

xref = x; yref = y; uref = u; Kref = K;

sk = 1;
F = spalloc(Np,Np,1);
for i=0:N % old ordering
  for j=0:N - i
    if(i+j<=1), F(sk,sk) = 1.; end;
    sk = sk+1;
  end
end

hold on
for nlev=1:Nlevels
  lev = levels(nlev);

  xref = x; yref = y; uref = u; Kref = K; Jref = J;

  err = 1;
  while(err > tol)
    
    umin = min(uref, [], 1);
    umax = max(uref, [], 1);
    
    refineflag = (umin <= lev & umax >= lev);
    
    toref = find( refineflag);
    Nref = length(toref);

    uref = reshape(interp*uref(:,toref), Np, 4*Nref);
    xref = reshape(interp*xref(:,toref), Np, 4*Nref);
    yref = reshape(interp*yref(:,toref), Np, 4*Nref);
    
    Kref = 4*Nref;
    
    ufilt = V*(F*(invV*uref));
    err = max(max(abs(ufilt-uref)));

  end 

  ri = [-1;1;-1]; si = [-1;-1;1]; refNp = length(ri);
  interp1 = InterpMatrix2D(ri,si);
  xref = interp1*xref; yref = interp1*yref; uref = interp1*uref; 
  
  ltri = delaunay(ri,si);
  Nltri = size(ltri,1);
  tri = zeros(Kref*Nltri, Nfaces);
  ks = (1:Nltri)';
  for k=1:Kref
    tri(ks,:) = ltri + (k-1)*refNp;
    ks = ks + Nltri;
  end
  
  PlotContour2D(tri, xref(:), yref(:), uref(:), levels)
  
end
hold off
return
