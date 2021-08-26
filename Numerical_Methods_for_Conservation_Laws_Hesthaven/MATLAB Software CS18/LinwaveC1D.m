function [u] = LinwaveC1D(x,u,h,CFL,FinalTime)
% function [u] = LinwaveC1D(x,u,h,CFL,FinalTime)
% Purpose  : Integrate 1D wave equation until using 2nd order central scheme.
time = 0; tstep = 0;

% Set timestep
k = CFL*h;

% integrate scheme
while (time<FinalTime)
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  u = u + k*LinwaveCrhs1D(x,u,h,k,1);
  time = time+k; tstep = tstep+1;
end
return