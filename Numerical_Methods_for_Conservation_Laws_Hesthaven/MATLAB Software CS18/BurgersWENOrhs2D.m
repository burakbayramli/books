function [du] = BurgersWENOrhs2D(x,y,u,hx,hy,k,m,Crec,dw,beta,maxvel); 
% function [du] = BurgersWENOrhs2D(x,y,u,hx,hy,k,m,Crec,dw,beta,maxvel); 
% Purpose: Evaluate right hand side for 2D Burgers equation 
%           using a WENO method
Nxy = size(x); Nx = Nxy(2); Ny = Nxy(1); du = zeros(Ny,Nx);

% Extend data and assign boundary conditions in x-direction
for i=1:Ny
  [xe,ue] = extend(x(i,:),u(i,:),hx,m,'P',0,'P',0);

  % define cell left and right interface values
  ul = zeros(Nx+2,1); ur = zeros(Nx+2,1);

  for j=1:Nx+2
    [ul(j),ur(j)] = WENO(xe(j:(j+2*(m-1))),ue(j:(j+2*(m-1))),m,Crec,dw,beta);
  end;

  % Update residual
  du(i,:) = - (BurgersLF(ur(2:Nx+1),ul(3:Nx+2),0,maxvel) ...
                 - BurgersLF(ur(1:Nx),ul(2:Nx+1),0,maxvel))/hx;
end

% Extend data and assign boundary conditions in y-direction
for j=1:Nx
  [xe,ue] = extend(y(:,j),u(:,j),hy,m,'P',0,'P',0);

  % define cell left and right interface values
  ul = zeros(Ny+2,1); ur = zeros(Ny+2,1);

  for i=1:Ny+2
    [ul(i),ur(i)] = WENO(xe(i:(i+2*(m-1))),ue(i:(i+2*(m-1))),m,Crec,dw,beta);
  end;

  % Update residual
  du(:,j) = du(:,j) - (BurgersLF(ur(2:Ny+1),ul(3:Ny+2),0,maxvel) ...
                - BurgersLF(ur(1:Ny),ul(2:Ny+1),0,maxvel))/hy;
end
return