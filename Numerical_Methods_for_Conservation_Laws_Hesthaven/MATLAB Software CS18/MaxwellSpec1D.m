function [q] = MaxwellSpec1D(x,q,ep,mu,N,L,CFL,FinalTime)
% function [q] = MaxwellSpec1D(x,q,ep,mu,N,L,CFL,FinalTime)
% Purpose  : Integrate 1D Maxwells equation until FinalTime using an
% spectral Fourier collocation scheme and 3rd order SSP-RK method.
time = 0; tstep = 0; h = L/(2*N+1);

% Set timestep
cvel = 1./sqrt(ep.*mu); k = CFL*h/max(cvel);

% integrate scheme
while (time<FinalTime)
  if (time+k>FinalTime) k = FinalTime-time; end
  
  % Update solution
  rhsq  = MaxwellSpecrhs1D(x, q,ep,mu,L); 
  q1 = q + k*rhsq;
  rhsq  = MaxwellSpecrhs1D(x,q1,ep,mu,L); 
  q2 = (3*q + q1 + k*rhsq)/4;
  rhsq  = MaxwellSpecrhs1D(x,q2,ep,mu,L); 
  q  = (q + 2*q2 + 2*k*rhsq)/3;
  time = time+k; 
end
return