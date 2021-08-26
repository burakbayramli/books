function [du] = BurgersSLrhs2D(x,y,u,hx,hy,k,maxvel)
% function [du] = BurgersSLrhs2D(x,y,u,hx,hy,k,maxvel);
% Purpose: Evaluate right hand side for 2D Burgers equation 
% using slope limited method
Nxy = size(x); Nx = Nxy(2); Ny = Nxy(1); du = zeros(Ny,Nx);

% Chose slope limiter - 0:LF; 1:minmod; 2:MUSCL; 3:Superbee; 
% 4:van Albada; 5:van Leer, 6: TVB 
type = 5; c=0; M=10;

% Extend data and assign boundary conditions in x-direction
for i=1:Ny
  [xe,ue] = extend(x(i,:),u(i,:),hx,2,'P',0,'P',0);
  
  % Compute element slopes and slope limit
  dup = ue(3:Nx+4)-ue(2:Nx+3); dum = ue(2:Nx+3)-ue(1:Nx+2);
  duL = SlopeLimit(dup,dum,type,c,M,hx);
  
  % Compute cell interface values 
  uloc = ue(2:Nx+3);
  uh = uloc - BurgersJac2Dx(uloc).*k/(2*hx).*duL; 
  uL = uh - duL/2; uR = uh + duL/2;
  
  % Compute rhs
  du(i,:) = -(BurgersLF(uR(2:Nx+1),uL(3:Nx+2),0,maxvel) ...
                - BurgersLF(uR(1:Nx),uL(2:Nx+1),0,maxvel))/hx;
end

% Extend data and assign boundary conditions in y-direction
for j=1:Nx
  [xe,ue] = extend(y(:,j),u(:,j),hy,2,'P',0,'P',0);
  
  % Compute element slopes and slope limit
  dup = ue(3:Ny+4)-ue(2:Ny+3); dum = ue(2:Ny+3)-ue(1:Ny+2);
  duL = SlopeLimit(dup,dum,type,c,M,hy);
  
  % Compute cell interface values - for f'(u) = 2*u;
  uloc = ue(2:Ny+3);
  uh = uloc - BurgersJac2Dy(uloc).*k/(2*hy).*duL; 
  uL = uh - duL/2; uR = uh + duL/2;
  
  % Compute rhs
  du(:,j) = du(:,j) - (BurgersLF(uR(2:Ny+1),uL(3:Ny+2),0,maxvel) ...
               - BurgersLF(uR(1:Ny),uL(2:Ny+1),0,maxvel))/hy;
end
return