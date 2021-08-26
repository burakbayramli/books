function [q] = MaxwellWENO1D(x,q,ep,mu,h,m,CFL,FinalTime)
% function [q] = MaxwellWENO1D(x,q,ep,mu,h,m,CFL,FinalTime)
% Purpose  : Integrate 1D Maxwells equation until FinalTime using a WENO
%            scheme and a 3rd order SSP-RK method.
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

% Set timestep
cvel = 1./sqrt(ep.*mu); k = CFL*h/max(cvel); maxvel = max(cvel);

% integrate scheme
while (time<FinalTime)
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  rhsq  = MaxwellWENOrhs1D(x, q,ep,mu,h,k,m,Crec,dw,beta,maxvel); 
  q1 = q + k*rhsq;
  rhsq  = MaxwellWENOrhs1D(x,q1,ep,mu,h,k,m,Crec,dw,beta,maxvel); 
  q2 = (3*q + q1 + k*rhsq)/4;
  rhsq  = MaxwellWENOrhs1D(x,q2,ep,mu,h,k,m,Crec,dw,beta,maxvel); 
  q  = (q + 2*q2 + 2*k*rhsq)/3;
  time = time+k; tstep = tstep+1;
end
return