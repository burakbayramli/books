function [q] = MaxwellENO1D(x,q,ep,mu,h,m,CFL,FinalTime)
% function [q] = MaxwellENO1D(x,q,ep,mu,h,m,CFL,FinalTime)
% Purpose  : Integrate 1D Maxwells equation until FinalTime using an ENO
%            scheme and 3rd order SSP-RK method.
time = 0; tstep = 0;

% Initialize reconstruction weights
Crec = zeros(m+1,m);
for r=-1:m-1;
    Crec(r+2,:) = ReconstructWeights(m,r);
end;

% Set timestep
cvel = 1./sqrt(ep.*mu); k = CFL*h/max(cvel); maxvel = max(cvel);

% integrate scheme
while (time<FinalTime)
  if (time+k>FinalTime) k = FinalTime-time; end
  
  % Update solution
  rhsq  = MaxwellENOrhs1D(x,q,ep,mu,h,k,m,Crec,maxvel); 
  q1 = q + k*rhsq;
  rhsq  = MaxwellENOrhs1D(x,q1,ep,mu,h,k,m,Crec,maxvel); 
  q2 = (3*q + q1 + k*rhsq)/4;
  rhsq  = MaxwellENOrhs1D(x,q2,ep,mu,h,k,m,Crec,maxvel); 
  q  = (q + 2*q2 + 2*k*rhsq)/3;
  time = time+k; tstep = tstep+1;
end
return