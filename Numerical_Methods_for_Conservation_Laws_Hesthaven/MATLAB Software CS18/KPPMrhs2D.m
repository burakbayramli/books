function [du] = KPPMrhs2D(x,y,u,hx,hy,k,maxvel); 
% function [du] = KPPMrhs2D(x,y,u,hx,hy,k,maxvel); 
% Purpose: Evaluate right hand side for 2D KPP equation 
% using a monotone method
Nxy = size(x); Nx = Nxy(2); Ny = Nxy(1); du = zeros(Ny,Nx);

% Extend data and assign boundary conditions in x-direction
for i=1:Ny
  [xe,ue] = extend(x(i,:),u(i,:),hx,1,'D',pi/4,'D',pi/4);
  % Update residual
  du(i,:) = -(KPPxLF(ue(2:Nx+1),ue(3:Nx+2),0,maxvel) - ...
                KPPxLF(ue(1:Nx),ue(2:Nx+1),0,maxvel))/hx;
end

% Extend data and assign boundary conditions in y-direction
for j=1:Nx
  [xe,ue] = extend(y(:,j),u(:,j),hy,1,'D',pi/4,'D',pi/4);
  % Update residual
  du(:,j) = du(:,j) - (KPPyLF(ue(2:Ny+1),ue(3:Ny+2),0,maxvel) - ...
              KPPyLF(ue(1:Ny),ue(2:Ny+1),0,maxvel))/hy;
end
return