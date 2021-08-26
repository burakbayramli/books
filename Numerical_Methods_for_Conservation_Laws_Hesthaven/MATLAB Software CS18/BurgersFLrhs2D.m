function [du] = BurgersFLrhs2D(x,y,u,hx,hy,k,maxvel)
% function [du] = BurgersFLrhs2D(x,y,u,hx,hy,k,maxvel);
% Purpose: Evaluate right hand side for 2D Burgers equation 
% using a flux limited scheme
Nxy = size(x); Nx = Nxy(2); Ny = Nxy(1); du = zeros(Ny,Nx);

% Chose flux limiter - 0:LF; 1:CO; 2:Koren; 3:Sweby; 4:OSPRE; 5:van Leer 
type = 5; beta = 1.5;

% Extend data and assign boundary conditions in x-direction
for i=1:Ny
  [xe,ue] = extend(x(i,:),u(i,:),hx,1,'P',0,'P',0);

   % Compute indicator function and define flux limiter
  r = (ue(2:Nx+1) - ue(1:Nx))./(ue(3:Nx+2)-ue(2:Nx+1));
  [xe,re] = extend(x(i,:),r,hx,1,'N',0,'N',0); rm = 1./re;
  phiLp = FluxLimit(re(1:Nx),type,beta); 
  phiRp = FluxLimit(re(2:Nx+1),type,beta);
  phiLm = FluxLimit(rm(2:Nx+1),type,beta); 
  phiRm = FluxLimit(rm(3:Nx+2),type,beta);
 
  ufilt = (u(i,:)'>=0);   
  phiL = ufilt.*phiLp + (1-ufilt).*phiLm; 
  phiR = ufilt.*phiRp + (1-ufilt).*phiRm;
 
  % Compute left flux - Change numerical flux here
  Fluxlow = BurgersLF(ue(1:Nx),ue(2:Nx+1),0,maxvel); 
  Fluxhigh = BurgersLW(ue(1:Nx),ue(2:Nx+1),k/hx,maxvel); 
  FluxL = Fluxlow - phiL.*(Fluxlow - Fluxhigh);
  
  % Compute right flux - Change numerical flux here
  Fluxlow = BurgersLF(ue(2:Nx+1),ue(3:Nx+2),0,maxvel); 
  Fluxhigh = BurgersLW(ue(2:Nx+1),ue(3:Nx+2),k/hx,maxvel); 
  FluxR = Fluxlow - phiR.*(Fluxlow - Fluxhigh);

  % Update residual 
  du(i,:) = -(FluxR' - FluxL')/hx;
end

% Extend data and assign boundary conditions in y-direction
for j=1:Nx
  [xe,ue] = extend(y(:,j),u(:,j),hy,1,'P',0,'P',0);

  % Compute indicator function and define flux limiter
  r = (ue(2:Ny+1) - ue(1:Ny))./(ue(3:Ny+2)-ue(2:Ny+1));
  [xe,re] = extend(y(:,j),r,hy,1,'N',0,'N',0); rm = 1./re;
  phiLp = FluxLimit(re(1:Ny),type,beta); 
  phiRp = FluxLimit(re(2:Ny+1),type,beta);
  phiLm = FluxLimit(rm(2:Ny+1),type,beta); 
  phiRm = FluxLimit(rm(3:Ny+2),type,beta);

  ufilt = (u(:,j)>=0); 
  phiL = ufilt.*phiLp + (1-ufilt).*phiLm; 
  phiR = ufilt.*phiRp + (1-ufilt).*phiRm;

  % Compute left flux - Change numerical flux here
  Fluxlow = BurgersLF(ue(1:Ny),ue(2:Ny+1),0,maxvel); 
  Fluxhigh = BurgersLW(ue(1:Ny),ue(2:Ny+1),k/hy,maxvel); 
  FluxL = Fluxlow - phiL.*(Fluxlow - Fluxhigh);
  
  % Compute right flux - Change numerical flux here
  Fluxlow = BurgersLF(ue(2:Ny+1),ue(3:Ny+2),0,maxvel); 
  Fluxhigh = BurgersLW(ue(2:Ny+1),ue(3:Ny+2),k/hy,maxvel); 
  FluxR = Fluxlow - phiR.*(Fluxlow - Fluxhigh);  
  
  % Update residual
  du(:,j) = du(:,j) - (FluxR - FluxL)/hy;
end
return