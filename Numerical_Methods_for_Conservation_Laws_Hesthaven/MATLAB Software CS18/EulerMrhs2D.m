function [dq] = EulerMrhs2D(x,y,q,gamma,hx,hy,k);
% function [dq] = EulerMrhs2D(x,y,q,gamma,hx,hy,k);
% Purpose: Evaluate right hand side for the two-dimensional  
% Euler equations using monotone method
Nxy = size(x); Nx = Nxy(2); Ny = Nxy(1); dq = zeros(Ny,Nx,4);
qex = zeros(Nx+2,4); qey = zeros(Ny+2,4);

% Apply monotone scheme in the x-direction
for i=1:Ny
  % Extend data and assign boundary conditions in x-direction
  [xe,qex(:,1)] = extend(x(i,:),q(i,:,1),hx,1,'N',0.0,'N',0.0); 
  [xe,qex(:,2)] = extend(x(i,:),q(i,:,2),hx,1,'N',0.0,'N',0.0);
  [xe,qex(:,3)] = extend(x(i,:),q(i,:,3),hx,1,'N',0.0,'N',0.0); 
  [xe,qex(:,4)] = extend(x(i,:),q(i,:,4),hx,1,'N',0.0,'N',0.0);
 
  % Compute flux 
  [dq1] = EulerLF2Dx(qex(2:Nx+1,:), qex(3:Nx+2,:),gamma);
  [dq2] = EulerLF2Dx(qex(1:Nx  ,:), qex(2:Nx+1,:),gamma);

  % Update residual
  dq(i,:,:) = -(dq1-dq2)/hx;
end

% Apply monotone scheme in the y-direction
for j=1:Nx
  % Extend data and assign boundary conditions in y-direction
  [ye,qey(:,1)] = extend(y(:,j),q(:,j,1),hy,1,'N',0.0,'N',0.0); 
  [ye,qey(:,2)] = extend(y(:,j),q(:,j,2),hy,1,'N',0.0,'N',0.0);
  [ye,qey(:,3)] = extend(y(:,j),q(:,j,3),hy,1,'N',0.0,'N',0.0); 
  [ye,qey(:,4)] = extend(y(:,j),q(:,j,4),hy,1,'N',0.0,'N',0.0);
  
  % Compute flux 
  [dq1] = EulerLF2Dy(qey(2:Ny+1,:), qey(3:Ny+2,:),gamma);
  [dq2] = EulerLF2Dy(qey(1:Ny  ,:), qey(2:Ny+1,:),gamma);

  % Update residual
  dq(:,j,:) = dq(:,j,:) - (reshape(dq1,Ny,1,4) ...
                 -reshape(dq2,Ny,1,4))/hy;
end
return