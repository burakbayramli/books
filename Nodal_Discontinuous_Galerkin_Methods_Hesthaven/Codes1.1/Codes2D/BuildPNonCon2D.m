function pinfo = BuildPNonCon2D(Norder, pK, pVX, pVY, pEToV, pBCType)

% function pinfo = BuildPNonCon2D(Norder, pK, pVX, pVY, pEToV, pBCType)
% Purpose: construct info necessary for DGTD on P-nonconforming meshes

Globals2D;

% Find maximum requested polynomial order
Nmax = max(Norder);

% Mesh details
Nfaces = 3; NODETOL = 1e-12; VX = pVX; VY = pVY;

kmap = zeros(pK,2);
sk = 1;

% Perform a mini StartUp2D for the elements of each order
for N=1:Nmax

  % load N'th order polynomial nodes
  Np = (N+1)*(N+2)/2; Nfp = N+1;
  [r,s] = Nodes2D(N); [r,s] = xytors(r,s);

  % Find list of N'th order nodes on each face of the reference element
  ids1 = find(abs(1+s)<NODETOL); [foo, ids] = sort(r(ids1), 'ascend');  ids1 = ids1(ids);
  ids2 = find(abs(r+s)<NODETOL); [foo, ids] = sort(r(ids2), 'descend'); ids2 = ids2(ids);
  ids3 = find(abs(1+r)<NODETOL); [foo, ids] = sort(s(ids3), 'descend'); ids3 = ids3(ids);
  Fmask = [ids1,ids2,ids3];

  % Build reference element matrices
  V = Vandermonde2D(N,r,s); invV = inv(V);
  MassMatrix = invV'*invV;
  [Dr,Ds] = Dmatrices2D(N, r, s, V);
  LIFT = Lift2D();    

  % store information for N'th order elements 
  pinfo(N).Np = Np; pinfo(N).Nfp = Nfp; pinfo(N).Fmask = Fmask; pinfo(N).r= r; pinfo(N).s=s; 
  pinfo(N).Dr=Dr; pinfo(N).Ds=Ds; pinfo(N).LIFT=LIFT; pinfo(N).V=V;

  % Find elements of polynomial order N
  ksN = find(Norder==N); K = length(ksN);
  if(K>0)

    % Use the subset of elements of order N
    EToV = pEToV(ksN,:); BCType = pBCType(ksN,:);
    kmap(ksN,1) = 1:K; kmap(ksN,2) = N;

    % Build coordinates of all the nodes
    va = EToV(:,1)'; vb = EToV(:,2)'; vc = EToV(:,3)';
    x = 0.5*(-(r+s)*VX(va)+(1+r)*VX(vb)+(1+s)*VX(vc));
    y = 0.5*(-(r+s)*VY(va)+(1+r)*VY(vb)+(1+s)*VY(vc));

    % Calculate geometric factors
    [rx,sx,ry,sy,J] = GeometricFactors2D(x,y,Dr,Ds);
    [nx, ny, sJ] = Normals2D();
    Fscale = sJ./(J(Fmask,:));
    
    % Calculate element connections on this mesh
    [EToE, EToF] = tiConnect2D(EToV);
    [mapM, mapP, vmapM, vmapP, vmapB, mapB] = BuildMaps2D();
    BuildBCMaps2D();
    pinfo(N).mapW = mapW;
    
    % Compute triangulation of N'th order nodes on mesh
    triN = delaunay(r, s);
    alltri = [];
    for k=1:K
      alltri = [alltri; triN+(k-1)*Np];
    end

    % Store geometric inforatmion in pinfo struct
    pinfo(N).rx = rx; pinfo(N).sx = sx; pinfo(N).ry = ry; pinfo(N).sy = sy; 
    pinfo(N).nx = nx; pinfo(N).ny = ny; pinfo(N).sJ = sJ; pinfo(N).J  = J; 
    pinfo(N).x  = x; pinfo(N).y = y; pinfo(N).Fscale = Fscale;

    pinfo(N).K = K; pinfo(N).ks = ksN; pinfo(N).tri = alltri;

    % Store location of the N'th order nodes in a global vector
    pinfo(N).ids = reshape(sk:sk+K*Np-1, Np, K);
    sk = sk+K*Np;
  end
end

% For each possible order
for N1=1:Nmax
  % generate face L2projection matrices (from order N2 to order N1 face space)
  for N2=1:Nmax

    % Set up sufficient Gauss quadrature to exactly perform surface integrals
    [gz, gw] = JacobiGQ(0, 0, max(N1,N2));
    
    % All edges have same distribution (note special Fmask)
    rM = pinfo(N1).r(pinfo(N1).Fmask(:,1));
    rP = pinfo(N2).r(pinfo(N2).Fmask(:,1));
    
    % Build N2 to N1 projection matrices for '+' trace data
    interpM = Vandermonde1D(N1, gz)/Vandermonde1D(N1, rM);
    interpP = Vandermonde1D(N2, gz(end:-1:1))/Vandermonde1D(N2, rP);
    
    % Face mass matrix used in projection
    mmM = interpM'*diag(gw)*interpM;
    
    pinfo(N1).interpP{N2} = mmM\(interpM'*diag(gw)*interpP);
  end
end

% Generate neighbor information for all faces 
[EToE, EToF] = tiConnect2D(pEToV);

% For each possible polynomial order  
for N1=1:Nmax
  
  % Create a set of indexing arrays, one for each possible neighbor order
  pinfo(N1).fmapM = cell(Nmax,1);
  pinfo(N1).vmapP = cell(Nmax,1);
  
  for N2=1:Nmax
    pinfo(N1).fmapM{N2} = [];
    pinfo(N1).vmapP{N2} = [];
  end

  % Loop through all elements of order N1
  for k1=1:pinfo(N1).K
    
    % Find element in original mesh
    k1orig = pinfo(N1).ks(k1);

    % Check all it's faces
    for f1=1:Nfaces
      % Find neighboring element (i.e. it's order and location in N2 mesh)
      k2orig = EToE(k1orig,f1);
      f2     = EToF(k1orig,f1);
      k2     = kmap(k2orig,1);
      N2     = kmap(k2orig,2);

      % Compute location of face nodes of '-' trace
      idsM = (k1-1)*pinfo(N1).Nfp*Nfaces + (f1-1)*pinfo(N1).Nfp;
      idsM = idsM+1:idsM+pinfo(N1).Nfp;

      % Find location of volume nodes on '+' trace of (k1orig,f1)
      idsP = pinfo(N2).ids(pinfo(N2).Fmask(:,f2), k2);

      % Store node locations in cell arrays
      pinfo(N1).fmapM{N2} = [pinfo(N1).fmapM{N2}, idsM];
      pinfo(N1).vmapP{N2} = [pinfo(N1).vmapP{N2}, idsP];

    end
  end
end
return;
