function [dq] = EulerCrhs2D(x,y,q,gamma,hx,hy,k);
% function [dq] = EulerCrhs2D(x,y,q,gamma,hx,hy,k);
% Purpose: Evaluate right hand side for the two-dimensional Euler equations 
%              using a second order central scheme
Nxy=size(x); Nx=Nxy(2); Ny=Nxy(1); Nxs=Nx-1; Nys=Ny-1; kl = k/2;
dq = zeros(Ny,Nx,4);

rG = zeros(Ny+4,Nx+4); ruG = zeros(Ny+4,Nx+4); rvG = zeros(Ny+4,Nx+4); EG = zeros(Ny+4,Nx+4); 
drLx = zeros(Ny+2,Nx+2); drLy = zeros(Ny+2,Nx+2); druLx = zeros(Ny+2,Nx+2); druLy = zeros(Ny+2,Nx+2);
drvLx = zeros(Ny+2,Nx+2); drvLy = zeros(Ny+2,Nx+2); dELx = zeros(Ny+2,Nx+2); dELy = zeros(Ny+2,Nx+2);

rGs = zeros(Nys+4,Nxs+4); ruGs = zeros(Nys+4,Nxs+4); rvGs = zeros(Nys+4,Nxs+4); EGs = zeros(Nys+4,Nxs+4); 
drLxs = zeros(Nys+2,Nxs+2); drLys = zeros(Nys+2,Nxs+2); druLxs = zeros(Nys+2,Nxs+2); druLys = zeros(Nys+2,Nxs+2);
drvLxs = zeros(Nys+2,Nxs+2); drvLys = zeros(Nys+2,Nxs+2); dELxs = zeros(Nys+2,Nxs+2); dELys = zeros(Nys+2,Nxs+2);

rhG = zeros(Ny+2,Nx+2); ruhG = zeros(Ny+2,Nx+2); rvhG = zeros(Ny+2,Nx+2); EhG = zeros(Ny+2,Nx+2);
rhGs = zeros(Nys+2,Nxs+2); ruhGs = zeros(Nys+2,Nxs+2); rvhGs = zeros(Nys+2,Nxs+2); EhGs = zeros(Nys+2,Nxs+2);

% Chose slope limiter - 0:LF; 1:minmod; 2:MUSCL; 3:Superbee; 
% 4:van Albada; 5:van Leer, 6: TVB 
type = 2; c=0; M=10;

% First step from non-staggered to staggered grid
% Extend data and assign boundary conditions on global data
for i=1:Ny
   [xe,re]  = extend(x(1,:),q(i,:,1),hx,2,'N',0,'N',0); rG (i+2,:) = re';
   [xe,rue] = extend(x(1,:),q(i,:,2),hx,2,'N',0,'N',0); ruG(i+2,:) = rue';
   [xe,rve] = extend(x(1,:),q(i,:,3),hx,2,'N',0,'N',0); rvG(i+2,:) = rve';
   [xe,Ee]  = extend(x(1,:),q(i,:,4),hx,2,'N',0,'N',0); EG (i+2,:) = Ee';
end
for j=1:Nx+4
   [xe,re]  = extend(y(:,1), rG(3:Ny+2,j),hy,2,'N',0,'N',0); rG (:,j) = re;
   [xe,rue] = extend(y(:,1),ruG(3:Ny+2,j),hy,2,'N',0,'N',0); ruG(:,j) = rue;
   [xe,rve] = extend(y(:,1),rvG(3:Ny+2,j),hy,2,'N',0,'N',0); rvG(:,j) = rve;
   [xe,Ee]  = extend(y(:,1), EG(3:Ny+2,j),hy,2,'N',0,'N',0); EG (:,j) = Ee;
end

% Compute slopes
for i=1:Ny+2;
   dup = rG(i+1,3:Nx+4)-rG(i+1,2:Nx+3); dum = rG(i+1,2:Nx+3) - rG(i+1,1:Nx+2); 
   drLx(i,:) = SlopeLimit(dup',dum',type,c,M,hx)';
   dup = ruG(i+1,3:Nx+4)-ruG(i+1,2:Nx+3); dum = ruG(i+1,2:Nx+3) - ruG(i+1,1:Nx+2); 
   druLx(i,:) = SlopeLimit(dup',dum',type,c,M,hx)';
   dup = rvG(i+1,3:Nx+4)-rvG(i+1,2:Nx+3); dum = rvG(i+1,2:Nx+3) - rvG(i+1,1:Nx+2); 
   drvLx(i,:) = SlopeLimit(dup',dum',type,c,M,hx)';
   dup = EG(i+1,3:Nx+4)-EG(i+1,2:Nx+3); dum = EG(i+1,2:Nx+3) - EG(i+1,1:Nx+2); 
   dELx(i,:) = SlopeLimit(dup',dum',type,c,M,hx)';
end
for j=1:Nx+2
   dup = rG(3:Ny+4,j+1)-rG(2:Ny+3,j+1); dum = rG(2:Ny+3,j+1) - rG(1:Ny+2,j+1);
   drLy(:,j) = SlopeLimit(dup,dum,type,c,M,hy);
   dup = ruG(3:Ny+4,j+1)-ruG(2:Ny+3,j+1); dum = ruG(2:Ny+3,j+1) - ruG(1:Ny+2,j+1);
   druLy(:,j) = SlopeLimit(dup,dum,type,c,M,hy);   
   dup = rvG(3:Ny+4,j+1)-rvG(2:Ny+3,j+1); dum = rvG(2:Ny+3,j+1) - rvG(1:Ny+2,j+1);
   drvLy(:,j) = SlopeLimit(dup,dum,type,c,M,hy);   
   dup = EG(3:Ny+4,j+1)-EG(2:Ny+3,j+1); dum = EG(2:Ny+3,j+1) - EG(1:Ny+2,j+1);
   dELy(:,j) = SlopeLimit(dup,dum,type,c,M,hy);
end

% Compute intermediate solution
for j=1:Nx+2
   for i=1:Ny+2
      rl = rG(i+1,j+1); rul = ruG(i+1,j+1); rvl = rvG(i+1,j+1); El = EG(i+1,j+1);
      Qq = [rl;rul;rvl;El];
      Ax = EulerJac2Dx(Qq,gamma); Ay = EulerJac2Dy(Qq,gamma);
      drLxl = drLx(i,j); druLxl = druLx(i,j); drvLxl = drvLx(i,j); dELxl = dELx(i,j);
      drLyl = drLy(i,j); druLyl = druLy(i,j); drvLyl = drvLy(i,j); dELyl = dELy(i,j);
      Qx = [drLxl; druLxl; drvLxl; dELxl]; Qy = [drLyl; druLyl; drvLyl; dELyl];
      Qhl = Qq - kl/2*(Ax*Qx/hx + Ay*Qy/hy);
      rhG(i,j) = Qhl(1); ruhG(i,j) = Qhl(2); rvhG(i,j) = Qhl(3); EhG(i,j)=Qhl(4);
   end
end
      
% Compute flux in x-direction
for i=1:Ny-1
  % Left term
  re  = rG (i+2,:)'; drL  = drLx (i+1,:)'; rh  = rhG(i+1,:)';
  rue = ruG(i+2,:)'; druL = druLx(i+1,:)'; ruh = ruhG(i+1,:)';
  rve = rvG(i+2,:)'; drvL = drvLx(i+1,:)'; rvh = rvhG(i+1,:)';
  Ee  = EG (i+2,:)'; dEL  = dELx (i+1,:)'; Eh  = EhG(i+1,:)';

  Qh = [rh ruh rvh Eh]; dQ = [drL druL drvL dEL]; Qe = [re rue rve Ee];
  dQp = EulerFlux2Dx(Qh(3:Nx+1,:), gamma); 
  dQm = EulerFlux2Dx(Qh(2:Nx,:), gamma); 

  Flux1 = (Qe(3:Nx+1,:)+Qe(4:Nx+2,:))/4 + (dQ(2:Nx,:)-dQ(3:Nx+1,:))/8 - kl/hx*(dQp - dQm);

  % Right term
  re  = rG (i+3,:)'; drL  = drLx (i+2,:)'; rh  = rhG(i+2,:)';
  rue = ruG(i+3,:)'; druL = druLx(i+2,:)'; ruh = ruhG(i+2,:)';
  rve = rvG(i+3,:)'; drvL = drvLx(i+2,:)'; rvh = rvhG(i+2,:)';
  Ee  = EG (i+3,:)'; dEL  = dELx (i+2,:)'; Eh  = EhG(i+2,:)';

  Qh = [rh ruh rvh Eh]; dQ = [drL druL drvL dEL]; Qe = [re rue rve Ee];
  dQp = EulerFlux2Dx(Qh(3:Nx+1,:), gamma); 
  dQm = EulerFlux2Dx(Qh(2:Nx,:), gamma); 

  Flux2 = (Qe(3:Nx+1,:)+Qe(4:Nx+2,:))/4 + (dQ(2:Nx,:)-dQ(3:Nx+1,:))/8 - kl/hx*(dQp - dQm);  

  % Compute rhs
  rs (i,:) = (Flux1(:,1)+Flux2(:,1))'/2;
  rus(i,:) = (Flux1(:,2)+Flux2(:,2))'/2;
  rvs(i,:) = (Flux1(:,3)+Flux2(:,3))'/2;
  Es (i,:) = (Flux1(:,4)+Flux2(:,4))'/2;
end  

% Compute flux in y-direction
for j=1:Nx-1
  % Down term
  re  = rG (:,j+2); drL  = drLy (:,j+1); rh  = rhG (:,j+1);
  rue = ruG(:,j+2); druL = druLy(:,j+1); ruh = ruhG(:,j+1);
  rve = rvG(:,j+2); drvL = drvLy(:,j+1); rvh = rvhG(:,j+1);
  Ee  = EG (:,j+2); dEL  = dELy (:,j+1); Eh  = EhG (:,j+1);

  Qh = [rh ruh rvh Eh]; dQ = [drL druL drvL dEL]; Qe = [re rue rve Ee];
  dQp = EulerFlux2Dy(Qh(3:Ny+1,:), gamma); 
  dQm = EulerFlux2Dy(Qh(2:Ny,:), gamma); 

  Flux1 = (Qe(3:Ny+1,:)+Qe(4:Ny+2,:))/4 + (dQ(2:Ny,:)-dQ(3:Ny+1,:))/8 - kl/hy*(dQp - dQm);

  % Right term
  re  = rG (:,j+3); drL  = drLy (:,j+2); rh  = rhG (:,j+2);
  rue = ruG(:,j+3); druL = druLy(:,j+2); ruh = ruhG(:,j+2);
  rve = rvG(:,j+3); drvL = drvLy(:,j+2); rvh = rvhG(:,j+2);
  Ee  = EG (:,j+3); dEL  = dELy (:,j+2); Eh  = EhG (:,j+2);

  Qh = [rh ruh rvh Eh]; dQ = [drL druL drvL dEL]; Qe = [re rue rve Ee];
  dQp = EulerFlux2Dy(Qh(3:Ny+1,:), gamma); 
  dQm = EulerFlux2Dy(Qh(2:Ny,:), gamma); 

  Flux2 = (Qe(3:Ny+1,:)+Qe(4:Ny+2,:))/4 + (dQ(2:Ny,:)-dQ(3:Ny+1,:))/8 - kl/hy*(dQp - dQm);

  % Compute rhs
  rs (:,j) = rs(:,j)  + (Flux1(:,1)+Flux2(:,1))/2;
  rus(:,j) = rus(:,j) + (Flux1(:,2)+Flux2(:,2))/2;
  rvs(:,j) = rvs(:,j) + (Flux1(:,3)+Flux2(:,3))/2;
  Es (:,j) = Es(:,j)  + (Flux1(:,4)+Flux2(:,4))/2;  
end   

% Second step from staggered to non-staggered grid
% Extend data and assign boundary conditions on global data
for i=1:Nys
   [xe,re]  = extendstag(x(1,1:Nxs),rs (i,:),hx,2,'N',0,'N',0); rGs(i+2,:) = re';
   [xe,rue] = extendstag(x(1,1:Nxs),rus(i,:),hx,2,'N',0,'N',0); ruGs(i+2,:) = rue';
   [xe,rve] = extendstag(x(1,1:Nxs),rvs(i,:),hx,2,'N',0,'N',0); rvGs(i+2,:) = rve';
   [xe,Ee]  = extendstag(x(1,1:Nxs),Es (i,:),hx,2,'N',0,'N',0); EGs(i+2,:) = Ee';
end
for j=1:Nxs+4
   [xe,re]  = extendstag(y(1:Nys,1),rGs (3:Nys+2,j),hy,2,'N',0,'N',0); rGs (:,j) = re;
   [xe,rue] = extendstag(y(1:Nys,1),ruGs(3:Nys+2,j),hy,2,'N',0,'N',0); ruGs(:,j) = rue;
   [xe,rve] = extendstag(y(1:Nys,1),rvGs(3:Nys+2,j),hy,2,'N',0,'N',0); rvGs(:,j) = rve;
   [xe,Ee]  = extendstag(y(1:Nys,1),EGs (3:Nys+2,j),hy,2,'N',0,'N',0); EGs (:,j) = Ee;
end

% Compute slopes
for i=1:Nys+2;
   dup = rGs(i+1,3:Nxs+4)-rGs(i+1,2:Nxs+3); dum = rGs(i+1,2:Nxs+3) - rGs(i+1,1:Nxs+2); 
   drLxs(i,:) = SlopeLimit(dup',dum',type,c,M,hx)';
   dup = ruGs(i+1,3:Nxs+4)-ruGs(i+1,2:Nxs+3); dum = ruGs(i+1,2:Nxs+3) - ruGs(i+1,1:Nxs+2); 
   druLxs(i,:) = SlopeLimit(dup',dum',type,c,M,hx)';   
   dup = rvGs(i+1,3:Nxs+4)-rvGs(i+1,2:Nxs+3); dum = rvGs(i+1,2:Nxs+3) - rvGs(i+1,1:Nxs+2); 
   drvLxs(i,:) = SlopeLimit(dup',dum',type,c,M,hx)';   
   dup = EGs(i+1,3:Nxs+4)-EGs(i+1,2:Nxs+3); dum = EGs(i+1,2:Nxs+3) - EGs(i+1,1:Nxs+2); 
   dELxs(i,:) = SlopeLimit(dup',dum',type,c,M,hx)';    
end
for j=1:Nxs+2
   dup = rGs(3:Nys+4,j+1)-rGs(2:Nys+3,j+1); dum = rGs(2:Nys+3,j+1) - rGs(1:Nys+2,j+1);
   drLys(:,j) = SlopeLimit(dup,dum,type,c,M,hy);
   dup = ruGs(3:Nys+4,j+1)-ruGs(2:Nys+3,j+1); dum = ruGs(2:Nys+3,j+1) - ruGs(1:Nys+2,j+1);
   druLys(:,j) = SlopeLimit(dup,dum,type,c,M,hy);
   dup = rvGs(3:Nys+4,j+1)-rvGs(2:Nys+3,j+1); dum = rvGs(2:Nys+3,j+1) - rvGs(1:Nys+2,j+1);
   drvLys(:,j) = SlopeLimit(dup,dum,type,c,M,hy);
   dup = EGs(3:Nys+4,j+1)-EGs(2:Nys+3,j+1); dum = EGs(2:Nys+3,j+1) - EGs(1:Nys+2,j+1);
   dELys(:,j) = SlopeLimit(dup,dum,type,c,M,hy);
end

% Compute intermediate solution
for j=1:Nxs+2
   for i=1:Nys+2
      rl = rGs(i+1,j+1); rul = ruGs(i+1,j+1); rvl = rvGs(i+1,j+1); El = EGs(i+1,j+1);
      Qq = [rl;rul;rvl;El];
      Ax = EulerJac2Dx(Qq,gamma); Ay = EulerJac2Dy(Qq,gamma);
      drLxl = drLxs(i,j); druLxl = druLxs(i,j); drvLxl = drvLxs(i,j); dELxl = dELxs(i,j);
      drLyl = drLys(i,j); druLyl = druLys(i,j); drvLyl = drvLys(i,j); dELyl = dELys(i,j);
      Qx = [drLxl; druLxl; drvLxl; dELxl]; Qy = [drLyl; druLyl; drvLyl; dELyl];
      Qhl = Qq - kl/2*(Ax*Qx/hx + Ay*Qy/hy);
      rhGs(i,j) = Qhl(1); ruhGs(i,j) = Qhl(2); rvhGs(i,j) = Qhl(3); EhGs(i,j)=Qhl(4);
   end
end

% Compute flux in x-direction
for i=1:Nys+1
  % Left term
  re  = rGs (i+1,:)'; drL  = drLxs (i,:)'; rh  = rhGs(i,:)';
  rue = ruGs(i+1,:)'; druL = druLxs(i,:)'; ruh = ruhGs(i,:)';
  rve = rvGs(i+1,:)'; drvL = drvLxs(i,:)'; rvh = rvhGs(i,:)';
  Ee  = EGs (i+1,:)'; dEL  = dELxs (i,:)'; Eh  = EhGs(i,:)';

  Qh = [rh ruh rvh Eh]; dQ = [drL druL drvL dEL]; Qe = [re rue rve Ee];
  dQp = EulerFlux2Dx(Qh(2:Nxs+2,:), gamma); 
  dQm = EulerFlux2Dx(Qh(1:Nxs+1,:), gamma); 

  Flux1 = (Qe(2:Nxs+2,:)+Qe(3:Nxs+3,:))/4 + (dQ(1:Nxs+1,:)-dQ(2:Nxs+2,:))/8 - kl/hx*(dQp - dQm);

  % Right term
  re  = rGs (i+2,:)'; drL  = drLxs (i+1,:)'; rh  = rhGs(i+1,:)';
  rue = ruGs(i+2,:)'; druL = druLxs(i+1,:)'; ruh = ruhGs(i+1,:)';
  rve = rvGs(i+2,:)'; drvL = drvLxs(i+1,:)'; rvh = rvhGs(i+1,:)';
  Ee  = EGs (i+2,:)'; dEL  = dELxs (i+1,:)'; Eh  = EhGs(i+1,:)';

  Qh = [rh ruh rvh Eh]; dQ = [drL druL drvL dEL]; Qe = [re rue rve Ee];
  dQp = EulerFlux2Dx(Qh(2:Nxs+2,:), gamma); 
  dQm = EulerFlux2Dx(Qh(1:Nxs+1,:), gamma); 

  Flux2 = (Qe(2:Nxs+2,:)+Qe(3:Nxs+3,:))/4 + (dQ(1:Nxs+1,:)-dQ(2:Nxs+2,:))/8 - kl/hx*(dQp - dQm);  

  % Compute rhs
  rns (i,:) = (Flux1(:,1)+Flux2(:,1))'/2;
  runs(i,:) = (Flux1(:,2)+Flux2(:,2))'/2;
  rvns(i,:) = (Flux1(:,3)+Flux2(:,3))'/2;
  Ens (i,:) = (Flux1(:,4)+Flux2(:,4))'/2;
end  

% Compute flux in y-direction
for j=1:Nxs+1
  % Down term
  re  = rGs (:,j+1); drL  = drLys (:,j); rh  = rhGs (:,j);
  rue = ruGs(:,j+1); druL = druLys(:,j); ruh = ruhGs(:,j);
  rve = rvGs(:,j+1); drvL = drvLys(:,j); rvh = rvhGs(:,j);
  Ee  = EGs (:,j+1); dEL  = dELys (:,j); Eh  = EhGs (:,j);

  Qh = [rh ruh rvh Eh]; dQ = [drL druL drvL dEL]; Qe = [re rue rve Ee];
  dQp = EulerFlux2Dy(Qh(2:Nys+2,:), gamma); 
  dQm = EulerFlux2Dy(Qh(1:Nys+1,:), gamma); 

  Flux1 = (Qe(2:Nys+2,:)+Qe(3:Nys+3,:))/4 + (dQ(1:Nys+1,:)-dQ(2:Nys+2,:))/8 - kl/hy*(dQp - dQm);

  % Right term
  re  = rGs (:,j+2); drL  = drLys (:,j+1); rh  = rhGs (:,j+1);
  rue = ruGs(:,j+2); druL = druLys(:,j+1); ruh = ruhGs(:,j+1);
  rve = rvGs(:,j+2); drvL = drvLys(:,j+1); rvh = rvhGs(:,j+1);
  Ee  = EGs (:,j+2); dEL  = dELys (:,j+1); Eh  = EhGs (:,j+1);

  Qh = [rh ruh rvh Eh]; dQ = [drL druL drvL dEL]; Qe = [re rue rve Ee];
  dQp = EulerFlux2Dy(Qh(2:Nys+2,:), gamma); 
  dQm = EulerFlux2Dy(Qh(1:Nys+1,:), gamma); 

  Flux2 = (Qe(2:Nys+2,:)+Qe(3:Nys+3,:))/4 + (dQ(1:Nys+1,:)-dQ(2:Nys+2,:))/8 - kl/hy*(dQp - dQm);

  % Compute rhs
  rns (:,j) = rns(:,j)  + (Flux1(:,1)+Flux2(:,1))/2;
  runs(:,j) = runs(:,j) + (Flux1(:,2)+Flux2(:,2))/2;
  rvns(:,j) = rvns(:,j) + (Flux1(:,3)+Flux2(:,3))/2;
  Ens (:,j) = Ens(:,j)  + (Flux1(:,4)+Flux2(:,4))/2;  
end

% Restore residual
dq(:,:,1) = (rns - q(:,:,1))/k; dq(:,:,2) = (runs - q(:,:,2))/k; 
dq(:,:,3) = (rvns - q(:,:,3))/k; dq(:,:,4) = (Ens - q(:,:,4))/k; 
return