function [dq] = EulerSLrhs2D(x,y,q,gamma,hx,hy,k); 
% function [dq] = EulerSLrhs2D(x,y,gamma,hx,hy,k);
% Purpose: Evaluate right hand side for the two-dimensional Euler equations 
%              using a slope limited scheme
Nxy = size(x); Nx = Nxy(2); Ny = Nxy(1); 
qex = zeros(Nx+4,4); qey = zeros(Ny+4,4); 
dqLx = zeros(Nx+2,4); dqLy = zeros(Ny+2,4);

% Chose slope limiter - 0:LF; 1:minmod; 2:MUSCL; 3:Superbee; 
% 4:van Albada; 5:van Leer, 6: TVB 
type = 2; c=0; M=10;

% Apply slope limited scheme in the x-direction
for i=1:Ny
  % Extend data and assign boundary conditions in x-direction
  [xe,qex(:,1)] = extend(x(i,:),q(i,:,1),hx,2,'N',0.0,'N',0.0); 
  [xe,qex(:,2)] = extend(x(i,:),q(i,:,2),hx,2,'N',0.0,'N',0.0);
  [xe,qex(:,3)] = extend(x(i,:),q(i,:,3),hx,2,'N',0.0,'N',0.0); 
  [xe,qex(:,4)] = extend(x(i,:),q(i,:,4),hx,2,'N',0.0,'N',0.0);
  
  % Compute element slopes and slope limit
  dup = qex(3:Nx+4,:) - qex(2:Nx+3,:);
  dum = qex(2:Nx+3,:) - qex(1:Nx+2,:);
  dqLx(:,1) = SlopeLimit(dup(:,1),dum(:,1),type,c,M,hx);
  dqLx(:,2) = SlopeLimit(dup(:,2),dum(:,2),type,c,M,hx);
  dqLx(:,3) = SlopeLimit(dup(:,3),dum(:,3),type,c,M,hx);
  dqLx(:,4) = SlopeLimit(dup(:,4),dum(:,4),type,c,M,hx);
  ql = qex(2:Nx+3,:) - dqLx/2; qr = qex(2:Nx+3,:) + dqLx/2;

  % Compute fluxes
  [dq1] = EulerLF2Dx(qr(2:Nx+1,:), ql(3:Nx+2,:),gamma);
  [dq2] = EulerLF2Dx(qr(1:Nx  ,:), ql(2:Nx+1,:),gamma);
  
  % Update residual
  dq(i,:,:) = -(dq1-dq2)/hx;
end

% Apply slope limited in the y-direction
for j=1:Nx
  % Extend data and assign boundary conditions in y-direction
  [ye,qey(:,1)] = extend(y(:,j),q(:,j,1),hy,2,'N',0.0,'N',0.0); 
  [ye,qey(:,2)] = extend(y(:,j),q(:,j,2),hy,2,'N',0.0,'N',0.0);
  [ye,qey(:,3)] = extend(y(:,j),q(:,j,3),hy,2,'N',0.0,'N',0.0); 
  [ye,qey(:,4)] = extend(y(:,j),q(:,j,4),hy,2,'N',0.0,'N',0.0);
  
  % Compute element slopes and slope limit
  dup = qey(3:Ny+4,:) - qey(2:Ny+3,:);
  dum = qey(2:Nx+3,:) - qey(1:Ny+2,:);
  dqLy(:,1) = SlopeLimit(dup(:,1),dum(:,1),type,c,M,hy);
  dqLy(:,2) = SlopeLimit(dup(:,2),dum(:,2),type,c,M,hy);
  dqLy(:,3) = SlopeLimit(dup(:,3),dum(:,3),type,c,M,hy);
  dqLy(:,4) = SlopeLimit(dup(:,4),dum(:,4),type,c,M,hy);
  ql = qey(2:Ny+3,:) - dqLy/2; qr = qey(2:Ny+3,:) + dqLy/2;

  % Compute fluxes
  [dq1] = EulerLF2Dy(qr(2:Ny+1,:), ql(3:Ny+2,:),gamma);
  [dq2] = EulerLF2Dy(qr(1:Ny  ,:), ql(2:Ny+1,:),gamma);

  % Update residual  
  dq(:,j,:) = dq(:,j,:) - (reshape(dq1,Ny,1,4) ...
                 -reshape(dq2,Ny,1,4))/hy;
end
return