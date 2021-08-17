function [LQ] = EulerLimiter2D(Q, SolutionBC, time)

% function [LQ] = EulerLimiter2D(Q, SolutionBC, time)
% Purpose: limit the Euler solution using slope limiting adapted from 
% A SLOPE LIMITING PROCEDURE IN DISCONTINUOUS GALERKIN FINITE ELEMENT METHOD FOR 
% GASDYNAMICS APPLICATIONS. SHUANGZHANG TU AND SHAHROUZ ALIABADI 
% INTERNATIONAL JOURNAL OF NUMERICAL ANALYSIS AND MODELING, Volume 2, Number 2, Pages 163

Globals2D;

% Gas constant
gamma = 1.4;

% 1. compute geometric information for 4 element patch containing each element
% Build average matrix
AVE = sum(MassMatrix)/2;

% Compute displacements from center of nodes for Taylor expansion of limited fields
dropAVE = eye(Np)-ones(Np,1)*AVE;
dx   = dropAVE*x; dy = dropAVE*y;

% Find neighbors in patch
E1 = EToE(:,1)'; E2 = EToE(:,2)'; E3 = EToE(:,3)';

% Extract coordinates of vertices and centers of elements
v1 = EToV(:,1); xv1 = VX(v1); yv1 = VY(v1);
v2 = EToV(:,2); xv2 = VX(v2); yv2 = VY(v2);
v3 = EToV(:,3); xv3 = VX(v3); yv3 = VY(v3);  

% compute face unit normals and lengths
fnx = [yv2-yv1;yv3-yv2;yv1-yv3]; fny = -[xv2-xv1;xv3-xv2;xv1-xv3];
fL = sqrt(fnx.^2 + fny.^2); fnx = fnx./fL; fny = fny./fL;

% compute element centers
xc0 = AVE*x; xc1 = xc0(E1); xc2 = xc0(E2); xc3 = xc0(E3);
yc0 = AVE*y; yc1 = yc0(E1); yc2 = yc0(E2); yc3 = yc0(E3);

% Compute weights for face gradients 
A0 = AVE*J*2/3; A1 = A0+A0(E1); A2 = A0+A0(E2); A3 = A0+A0(E3);

% Find boundary faces for each face 
id1 = find(BCType(:,1)); id2 = find(BCType(:,2)); id3 = find(BCType(:,3));

% Compute location of centers of reflected ghost elements at boundary faces
H1 = 2*(A0(id1)./fL(1,id1)); 
xc1(id1) = xc1(id1) + 2*fnx(1,id1).*H1; yc1(id1) = yc1(id1) + 2*fny(1,id1).*H1; 

H2 = 2*(A0(id2)./fL(2,id2)); 
xc2(id2) = xc2(id2) + 2*fnx(2,id2).*H2; yc2(id2) = yc2(id2) + 2*fny(2,id2).*H2; 

H3 = 2*(A0(id3)./fL(3,id3)); 
xc3(id3) = xc3(id3) + 2*fnx(3,id3).*H3; yc3(id3) = yc3(id3) + 2*fny(3,id3).*H3; 

% 2. Find cell averages of conserved & primitive variables in each 4 element patch
% extract fields from Q
rho = Q(:,:,1); rhou = Q(:,:,2); rhov = Q(:,:,3); Ener = Q(:,:,4);

% Compute cell averages of conserved variables
rhoC = AVE*rho; rhouC = AVE*rhou; rhovC = AVE*rhov; EnerC = AVE*Ener;
averhou = ones(Np,1)*rhouC; averhov = ones(Np,1)*rhovC;
averho  = ones(Np,1)*rhoC;  aveEner = ones(Np,1)*EnerC;

% Compute primitive variables from cell averages of conserved variables
PC0(1,:,1)=rhoC; PC0(1,:,2)= rhouC./rhoC; PC0(1,:,3)=rhovC./rhoC;  
PC0(1,:,4)=(gamma-1)*(EnerC - 0.5*(rhouC.^2 + rhovC.^2)./rhoC);

% Find neighbor values of conserved variables
PC(:,:,1)=rhoC(EToE'); PC(:,:,2)=rhouC(EToE'); PC(:,:,3)=rhovC(EToE'); PC(:,:,4)=EnerC(EToE');

% Find boundary faces
idW = find(BCType'==Wall); idI = find(BCType'==In); 
idO = find(BCType'==Out); idC = find(BCType'==Cyl);

% Apply boundary conditions to cell averages of ghost cells at boundary faces
PC = feval(SolutionBC, [xc1;xc2;xc3], [yc1;yc2;yc3], fnx, fny, idI, idO, idW, idC, PC, time);
PC(:,:,2) = PC(:,:,2)./PC(:,:,1); PC(:,:,3) = PC(:,:,3)./PC(:,:,1); 
PC(:,:,4) = (gamma-1)*(PC(:,:,4)-0.5*PC(:,:,1).*(PC(:,:,2).^2 + PC(:,:,3).^2));

% 3. Compute average of primitive variables at face nodes
ids = [1;Nfp;Nfp+1;2*Nfp;3*Nfp;2*Nfp+1]; 
vmapP1 = reshape(vmapP, Nfp*Nfaces, K); vmapP1 = vmapP1(ids, :);
vmapM1 = reshape(vmapM, Nfp*Nfaces, K); vmapM1 = vmapM1(ids, :);

rhoA  = ( rho(vmapP1)+ rho(vmapM1))/2; EnerA = (Ener(vmapP1)+Ener(vmapM1))/2;
rhouA = (rhou(vmapP1)+rhou(vmapM1))/2; rhovA = (rhov(vmapP1)+rhov(vmapM1))/2;

uA = rhouA./rhoA; vA = rhovA./rhoA; pA = (gamma-1)*(EnerA - 0.5*rhoA.*(uA.^2 + vA.^2));

PVA(:,:,1) = rhoA; PVA(:,:,2) = uA; PVA(:,:,3) = vA; PVA(:,:,4) = pA; 

% 4. Apply limiting procedure to each of the primitive variables

% Storage for cell averagse and limited gradients at each node of each element
aV = zeros(Np,K,4); dV = zeros(Np,K,4);

% Loop over primitive variables
for n=1:4

  % find value of primitive variables in patches
  VC0 = PC0(1,:,n); VC1 = PC(1,:,n); VC2 = PC(2,:,n); VC3 = PC(3,:,n); VA = PVA(:,:,n);

  % Compute face gradients
  dVdxE1 =  0.5.*( (VC1-VC0).*(yv2-yv1) + (VA(1,:)-VA(2,:)).*(yc1 - yc0) )./A1;
  dVdyE1 = -0.5.*( (VC1-VC0).*(xv2-xv1) + (VA(1,:)-VA(2,:)).*(xc1 - xc0) )./A1;
  dVdxE2 =  0.5.*( (VC2-VC0).*(yv3-yv2) + (VA(3,:)-VA(4,:)).*(yc2 - yc0) )./A2;
  dVdyE2 = -0.5.*( (VC2-VC0).*(xv3-xv2) + (VA(3,:)-VA(4,:)).*(xc2 - xc0) )./A2;
  dVdxE3 =  0.5.*( (VC3-VC0).*(yv1-yv3) + (VA(5,:)-VA(6,:)).*(yc3 - yc0) )./A3;
  dVdyE3 = -0.5.*( (VC3-VC0).*(xv1-xv3) + (VA(5,:)-VA(6,:)).*(xc3 - xc0) )./A3;

  dVdxC0 = (A1.*dVdxE1 + A2.*dVdxE2 + A3.*dVdxE3)./(A1+A2+A3);
  dVdyC0 = (A1.*dVdyE1 + A2.*dVdyE2 + A3.*dVdyE3)./(A1+A2+A3);
  
  dVdxC1 = dVdxC0(E1); dVdxC2 = dVdxC0(E2); dVdxC3 = dVdxC0(E3);
  dVdyC1 = dVdyC0(E1); dVdyC2 = dVdyC0(E2); dVdyC3 = dVdyC0(E3);

  % Use face gradients at ghost elements
  dVdxC1(id1) = dVdxE1(id1); dVdxC2(id2) = dVdxE2(id2); dVdxC3(id3) = dVdxE3(id3);
  dVdyC1(id1) = dVdyE1(id1); dVdyC2(id2) = dVdyE2(id2); dVdyC3(id3) = dVdyE3(id3);

  % Build weights used in limiting
  g1 = (dVdxC1.^2 + dVdyC1.^2); g2 = (dVdxC2.^2 + dVdyC2.^2); g3 = (dVdxC3.^2 + dVdyC3.^2);
  
  epse = 1e-10; fac = g1.^2 + g2.^2 + g3.^2;
  w1 = (g2.*g3+epse)./(fac+3*epse);
  w2 = (g1.*g3+epse)./(fac+3*epse);
  w3 = (g1.*g2+epse)./(fac+3*epse);
  
  % Limit gradients
  LdVdxC0 = w1.*dVdxC1 + w2.*dVdxC2 + w3.*dVdxC3;
  LdVdyC0 = w1.*dVdyC1 + w2.*dVdyC2 + w3.*dVdyC3;

  % Evaluate limited gradient and cell averages at all nodes of each element
  dV(:,:,n) = dx.*(ones(Np,1)*LdVdxC0) + dy.*(ones(Np,1)*LdVdyC0);
  aV(:,:,n) = ones(Np,1)*VC0;
end

% 4. Reconstruct conserved variables using cell averages and limited gradients  
averho  = aV(:,:,1); aveu = aV(:,:,2); avev = aV(:,:,3); avep = aV(:,:,4);
drho    = dV(:,:,1);   du = dV(:,:,2);   dv = dV(:,:,3);   dp = dV(:,:,4);

% Reconstruct and check for small densities and/or pressures
tol = 1e-2;

Lrho = averho + drho; ids = find(min(Lrho,[],1) < tol);
while(~isempty(ids))
  disp('warning: correcting negative density')
  drho(:,ids) = .5*drho(:,ids);
  Lrho = averho + drho; ids = find(min(Lrho,[],1) < tol);
end

% Reconstruct momentum
Lrhou = averhou + averho.*du + drho.*aveu; Lrhov = averhov + averho.*dv + drho.*avev;

% Reconstruct energy
dEner = (1/(gamma-1))*dp+0.5*drho.*(aveu.^2+avev.^2)+averho.*(aveu.*du+avev.*dv);
LEner = aveEner + dEner;

% Check for negative pressures => zero gradient 
Lp    = (gamma-1)*(LEner - 0.5*(Lrhou.^2 + Lrhov.^2)./Lrho);
ids = find(min(Lp,[],1) < tol);
if(~isempty(ids))
  disp('warning: correcting negative pressure');
  Lp(:,ids) = aveEner(:,ids);
end

% Replace limited gradients with face gradient at boundary faces
LQ = zeros(Np, K, 4);
LQ(:,:,1) = Lrho; LQ(:,:,2) = Lrhou; LQ(:,:,3) = Lrhov; LQ(:,:,4) = LEner;
return;
