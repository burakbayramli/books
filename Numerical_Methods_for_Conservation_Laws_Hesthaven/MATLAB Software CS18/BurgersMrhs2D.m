function [du] = BurgersMrhs2D(x,y,u,hx,hy,k,maxvel)
% function [du] = BurgersMrhs2D(x,y,u,hx,hy,k,maxvel);
% Purpose: Evaluate right hand side for 2D Burgers equation 
% using a monotone scheme
Nxy = size(x); Nx = Nxy(2); Ny = Nxy(1); du = zeros(Ny,Nx);

% Extend data and assign boundary conditions in x-direction
for i=1:Ny
  [xe,ue] = extend(x(i,:),u(i,:),hx,1,'P',0,'P',0);
  % Update residual
  du(i,:) = -(BurgersLF(ue(2:Nx+1),ue(3:Nx+2),0,maxvel) - ...
                  BurgersLF(ue(1:Nx),ue(2:Nx+1),0,maxvel))/hx;
end

% Extend data and assign boundary conditions in y-direction
for j=1:Nx
  [xe,ue] = extend(y(:,j),u(:,j),hy,1,'P',0,'P',0);
  % Update residual
  du(:,j) = du(:,j) - (BurgersLF(ue(2:Ny+1),ue(3:Ny+2),0,maxvel) - ...
                  BurgersLF(ue(1:Ny),ue(2:Ny+1),0,maxvel))/hy;
end
return