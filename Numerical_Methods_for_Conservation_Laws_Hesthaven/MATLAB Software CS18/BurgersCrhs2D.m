function [du] = BurgersCrhs2D(x,y,u,hx,hy,k,maxvel)
% function [du] = BurgersCrhs2D(x,y,u,hx,hy,k,maxvel);
% Purpose: Evaluate right hand side for 2D Burgers equation 
% using second order central scheme
Nxy=size(x); Nx=Nxy(2); Ny=Nxy(1); Nxs=Nx-1; Nys=Ny-1; kl = k/2;
du=zeros(Ny,Nx); duLx = zeros(Ny+2,Nx+2); duLy = zeros(Ny+2,Nx+2);
uG = zeros(Ny+4,Nx+4); uGs = zeros(Nys+4,Nxs+4); 
duLxs = zeros(Nys+2,Nxs+2); duLys = zeros(Nys+2,Nxs+2);

% Chose slope limiter - 0:LF; 1:minmod; 2:MUSCL; 3:Superbee; 
% 4:van Albada; 5:van Leer, 6: TVB 
type = 2; c=0; M=10;

% First step from non-staggered to staggered grid
% Extend data and assign boundary conditions on global data
for i=1:Ny
   [xe,ue] = extend(x(1,:),u(i,:),hx,2,'P',0,'P',0); 
   uG(i+2,:) = ue';
end
for j=1:Nx+4
   [xe,ue] = extend(y(:,1),uG(3:Ny+2,j),hy,2,'P',0,'P',0); 
   uG(:,j) = ue;
end

% Compute slopes
for i=1:Ny+2;
   dup = uG(i+1,3:Nx+4)-uG(i+1,2:Nx+3); 
   dum = uG(i+1,2:Nx+3)-uG(i+1,1:Nx+2); 
   duLx(i,:) = SlopeLimit(dup',dum',type,c,M,hx)';
end
for j=1:Nx+2
   dup = uG(3:Ny+4,j+1)-uG(2:Ny+3,j+1); 
   dum = uG(2:Ny+3,j+1)-uG(1:Ny+2,j+1);
   duLy(:,j) = SlopeLimit(dup,dum,type,c,M,hy);
end

% Compute intermediate solution
uloc = uG(2:Ny+3,2:Nx+3); 
uhG = uloc - kl/2.*(BurgersJac2Dx(uloc).*duLx/hx ...
                       + BurgersJac2Dy(uloc).*duLy/hy);

% Compute flux in x-direction
for i=1:Ny-1
  % Left term
  ue = uG(i+2,:)'; duL = duLx(i+1,:)'; uh = uhG(i+1,:)';
  Flux1 = (ue(3:Nx+1)+ue(4:Nx+2))/4 + (duL(2:Nx)-duL(3:Nx+1))/8 ...
      - kl/hx*(BurgersFlux2Dx(uh(3:Nx+1))-BurgersFlux2Dx(uh(2:Nx)));
  % Right term
  ue = uG(i+3,:)'; duL = duLx(i+2,:)'; uh = uhG(i+2,:)';
  Flux2 = (ue(3:Nx+1)+ue(4:Nx+2))/4 + (duL(2:Nx)-duL(3:Nx+1))/8 ...
      - kl/hx*(BurgersFlux2Dx(uh(3:Nx+1))-BurgersFlux2Dx(uh(2:Nx)));  
  % Compute rhs
  us(i,:) = (Flux1+Flux2)'/2;
end  

% Compute flux in y-direction
for j=1:Nx-1
  % Down term
  ue = uG(:,j+2); duL = duLy(:,j+1); uh = uhG(:,j+1);
  Flux1 = (ue(3:Ny+1)+ue(4:Ny+2))/4 + (duL(2:Ny)-duL(3:Ny+1))/8 ...
      - kl/hy*(BurgersFlux2Dy(uh(3:Ny+1))-BurgersFlux2Dy(uh(2:Ny)));
  % Up term
  ue = uG(:,j+3); duL = duLy(:,j+2); uh = uhG(:,j+2);
  Flux2 = (ue(3:Ny+1)+ue(4:Ny+2))/4 + (duL(2:Ny)-duL(3:Ny+1))/8 ...
      - kl/hy*(BurgersFlux2Dy(uh(3:Ny+1))-BurgersFlux2Dy(uh(2:Ny)));
  % Compute rhs
  us(:,j) = us(:,j)+(Flux1+Flux2)/2;
end

% Second step from staggered to non-staggered grid
% Extend data and assign boundary conditions on global data
for i=1:Nys
   [xe,ue]=extendstag(x(1,1:Nxs),us(i,:),hx,2,'P',0,'P',0); 
   uGs(i+2,:)=ue';
end
for j=1:Nxs+4
   [xe,ue]=extendstag(y(1:Nys,1),uGs(3:Nys+2,j),hy,2,'P',0,'P',0); 
   uGs(:,j)=ue;
end

% Compute slopes
for i=1:Nys+2;
   dup = uGs(i+1,3:Nxs+4)-uGs(i+1,2:Nxs+3); 
   dum = uGs(i+1,2:Nxs+3) - uGs(i+1,1:Nxs+2); 
   duLxs(i,:) = SlopeLimit(dup',dum',type,c,M,hx)';
end
for j=1:Nxs+2
   dup = uGs(3:Nys+4,j+1)-uGs(2:Nys+3,j+1); 
   dum = uGs(2:Nys+3,j+1) - uGs(1:Nys+2,j+1);
   duLys(:,j) = SlopeLimit(dup,dum,type,c,M,hy);
end
    
% Compute intermediate solution - 
uloc = uGs(2:Nys+3,2:Nxs+3);
uhGs = uloc - kl/2.*(BurgersJac2Dx(uloc).*duLxs/hx ...
                       + BurgersJac2Dy(uloc).*duLys/hy);

% Compute flux in x-direction
for i=1:Nys+1
  % Left term
  ue = uGs(i+1,:)'; duL = duLxs(i,:)'; uh = uhGs(i,:)';
  Flux1 = (ue(2:Nxs+2)+ue(3:Nxs+3))/4 + (duL(1:Nxs+1)-duL(2:Nxs+2))/8 ...
      - kl/hx*(BurgersFlux2Dx(uh(2:Nxs+2))-BurgersFlux2Dx(uh(1:Nxs+1)));
  % Right term
  ue = uGs(i+2,:)'; duL = duLxs(i+1,:)'; uh = uhGs(i+1,:)';
  Flux2 = (ue(2:Nxs+2)+ue(3:Nxs+3))/4 + (duL(1:Nxs+1)-duL(2:Nxs+2))/8 ...
      - kl/hx*(BurgersFlux2Dx(uh(2:Nxs+2))-BurgersFlux2Dx(uh(1:Nxs+1)));  
  % Compute rhs
  uns(i,:) = (Flux1+Flux2)'/2;
end  

% Compute flux in y-direction
for j=1:Nxs+1
  % Down term
  ue = uGs(:,j+1); duL = duLys(:,j); uh = uhGs(:,j);
  Flux1 = (ue(2:Nys+2)+ue(3:Nys+3))/4 + (duL(1:Nys+1)-duL(2:Nys+2))/8 ...
      - kl/hy*(BurgersFlux2Dy(uh(2:Nys+2))-BurgersFlux2Dy(uh(1:Nys+1)));
  % Up term
  ue = uGs(:,j+2); duL = duLys(:,j+1); uh = uhGs(:,j+1);
  Flux2 = (ue(2:Nys+2)+ue(3:Nys+3))/4 + (duL(1:Nys+1)-duL(2:Nys+2))/8 ...
      - kl/hy*(BurgersFlux2Dy(uh(2:Nys+2))-BurgersFlux2Dy(uh(1:Nys+1)));
  % Compute rhs
  uns(:,j) = uns(:,j)+(Flux1+Flux2)/2;
end

% Restore residual
du = (uns - u)/k;
return