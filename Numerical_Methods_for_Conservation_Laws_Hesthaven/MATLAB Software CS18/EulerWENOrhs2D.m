function [dq] = EulerWENOrhs2D(x,y,q,hx,hy,k,m,Crec,dw,beta,gamma);
% function [dq] = EulerWENOrhs2D(x,y,q,hx,hy,k,m,Crec,dw,beta,gamma);
% Purpose: Evaluate right hand side for the two-dimensional Euler equations 
%          using a WENO method
Nxy = size(x); Nx = Nxy(2); Ny = Nxy(1); dq = zeros(Ny,Nx,4); 

% Apply WENO in the x-direction
for i=1:Ny
  % Extend data and assign boundary conditions in x-direction
  [xe, re] = extend(x(i,:),q(i,:,1),hx,m,'N',0.0,'N',0.0); 
  [xe,rue] = extend(x(i,:),q(i,:,2),hx,m,'N',0.0,'N',0.0);
  [xe,rve] = extend(x(i,:),q(i,:,3),hx,m,'N',0.0,'N',0.0); 
  [xe, Ee] = extend(x(i,:),q(i,:,4),hx,m,'N',0.0,'N',0.0);

  % define cell left and right interface values
  ql = zeros(Nx+2,4); qr = zeros(Nx+2,4);
  for j=1:Nx+2
    [ql(j,1),qr(j,1)] = WENO(xe(j:(j+2*(m-1))), re(j:(j+2*(m-1))),m,Crec,dw,beta);
    [ql(j,2),qr(j,2)] = WENO(xe(j:(j+2*(m-1))),rue(j:(j+2*(m-1))),m,Crec,dw,beta);
    [ql(j,3),qr(j,3)] = WENO(xe(j:(j+2*(m-1))),rve(j:(j+2*(m-1))),m,Crec,dw,beta);
    [ql(j,4),qr(j,4)] = WENO(xe(j:(j+2*(m-1))), Ee(j:(j+2*(m-1))),m,Crec,dw,beta);
  end;
  
  % Compute flux 
  [dq1] = EulerLF2Dx(qr(2:Nx+1,:), ql(3:Nx+2,:),gamma);
  [dq2] = EulerLF2Dx(qr(1:Nx  ,:), ql(2:Nx+1,:),gamma);

  % Update residual
  dq(i,:,:) = -(dq1-dq2)/hx;
end

% Apply WENO in the y-direction
for j=1:Nx
  % Extend data and assign boundary conditions in y-direction
  [ye, re] = extend(y(:,j),q(:,j,1),hy,m,'N',0.0,'N',0.0); 
  [xe,rue] = extend(y(:,j),q(:,j,2),hy,m,'N',0.0,'N',0.0);
  [ye,rve] = extend(y(:,j),q(:,j,3),hy,m,'N',0.0,'N',0.0); 
  [xe, Ee] = extend(y(:,j),q(:,j,4),hy,m,'N',0.0,'N',0.0);

  % define cell left and right interface values
  ql = zeros(Ny+2,4); qr = zeros(Ny+2,4);
  for i=1:Ny+2
    [ql(i,1),qr(i,1)] = WENO(ye(i:(i+2*(m-1))), re(i:(i+2*(m-1))),m,Crec,dw,beta);
    [ql(i,2),qr(i,2)] = WENO(ye(i:(i+2*(m-1))),rue(i:(i+2*(m-1))),m,Crec,dw,beta);
    [ql(i,3),qr(i,3)] = WENO(ye(i:(i+2*(m-1))),rve(i:(i+2*(m-1))),m,Crec,dw,beta);
    [ql(i,4),qr(i,4)] = WENO(ye(i:(i+2*(m-1))), Ee(i:(i+2*(m-1))),m,Crec,dw,beta);
  end;
  
  % Compute flux 
  [dq1] = EulerLF2Dy(qr(2:Ny+1,:), ql(3:Ny+2,:),gamma);
  [dq2] = EulerLF2Dy(qr(1:Ny  ,:), ql(2:Ny+1,:),gamma);

  % Update residual
  dq(:,j,:) = dq(:,j,:) - (reshape(dq1,Ny,1,4) ...
                 -reshape(dq2,Ny,1,4))/hy;
end
return