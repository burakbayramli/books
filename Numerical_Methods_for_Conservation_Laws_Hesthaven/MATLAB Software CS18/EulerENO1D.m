function [q] = EulerENO1D(x,q,h,m,CFL,gamma,FinalTime)
% function [q] = EulerMDriver1D(x,q,h,m,CFL,gamma,FinalTime)
% Purpose  : Integrate 1D Euler equation until FinalTime using an ENO
%            scheme and a 3rd order SSP-RK
time = 0; tstep = 0; 

% Initialize reconstruction weights
Crec = zeros(m+1,m);
for r=-1:m-1;
    Crec(r+2,:) = ReconstructWeights(m,r);
end;

% integrate scheme
while (time<FinalTime)
    
  % Set timestep
  p = (gamma-1)*(q(:,3) - 0.5*q(:,2).^2./q(:,1)); c = sqrt(gamma*p./q(:,1)); 
  maxvel = max(c+abs(q(:,2)./q(:,1))); k = CFL*h/maxvel;
  if (time+k>FinalTime) k = FinalTime-time; end
    
  % Update solution
  rhsq  = EulerENOrhs1D(x, q,h,k,m,Crec,gamma,maxvel); 
  q1 = q + k*rhsq;
  rhsq  = EulerENOrhs1D(x,q1,h,k,m,Crec,gamma,maxvel); 
  q2 = (3*q + q1 + k*rhsq)/4;
  rhsq  = EulerENOrhs1D(x,q2,h,k,m,Crec,gamma,maxvel); 
  q  = (q + 2*q2 + 2*k*rhsq)/3;
  time = time+k; tstep = tstep+1;
end
return