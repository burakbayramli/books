function [EM] = MaxwellM1D(x,EM,ep,mu,h,CFL,FinalTime)
% function [EM] = MaxwellM1D(x,EM,ep,mu,CFL,FinalTime)
% Purpose  : Integrate 1D Maxwells equations until FinalTime using 
% a monotone scheme.
time = 0; tstep = 0; 

% Set timestep
cvel = 1./sqrt(ep.*mu); k = CFL*h/max(cvel);

% integrate scheme
while (time<FinalTime)
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  EM = EM + k*MaxwellMrhs1D(x,EM,ep,mu,h,k,max(cvel));
  time = time+k; tstep = tstep+1;
end
return