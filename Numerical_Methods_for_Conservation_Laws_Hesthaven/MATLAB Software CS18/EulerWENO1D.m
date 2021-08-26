function [q] = EulerWENO1D(x,q,h,m,CFL,gamma,FinalTime)
% function [q] = EulerWENODriver1D(x,q,h,m,CFL,gamma,FinalTime)
% Purpose  : Integrate 1D Euler equation until FinalTime using a WENO
%            scheme and a 3rd order SSP-RK
time = 0; tstep = 0;

% Initialize reconstruction weights
Crec = zeros(m+1,m);
for r=-1:m-1;
    Crec(r+2,:) = ReconstructWeights(m,r);
end;

% Initialize linear weights
dw = LinearWeights(m,0);

% Compute smoothness indicator matrices
beta = zeros(m,m,m);
for r=0:m-1
    xl = -1/2 + [-r:1:m-r];
    beta(:,:,r+1) = betarcalc(xl,m);
end

% integrate scheme
while (time<FinalTime)  
  % Set timestep
  p = (gamma-1)*(q(:,3) - 0.5*q(:,2).^2./q(:,1)); c = sqrt(gamma*p./q(:,1)); 
  maxvel = max(c+abs(q(:,2)./q(:,1))); k = CFL*h/maxvel;
  if (time+k>FinalTime) k = FinalTime-time; end
    
  % Update solution
  rhsq  = EulerWENOcharrhs1D(x, q,h,k,m,Crec,dw,beta,gamma,maxvel); 
  q1 = q + k*rhsq;
  rhsq  = EulerWENOcharrhs1D(x,q1,h,k,m,Crec,dw,beta,gamma,maxvel); 
  q2 = (3*q + q1 + k*rhsq)/4;
  rhsq  = EulerWENOcharrhs1D(x,q2,h,k,m,Crec,dw,beta,gamma,maxvel); 
  q  = (q + 2*q2 + 2*k*rhsq)/3;
  time = time+k; tstep = tstep+1;
end
return