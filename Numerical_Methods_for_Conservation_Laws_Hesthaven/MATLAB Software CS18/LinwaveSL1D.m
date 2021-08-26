function [u] = LinwaveSL1D(x,u,h,CFL,FinalTime)
% function [u] = LinwaveSL1D(x,u,h,CFL,FinalTime)
% Purpose  : Integrate 1D wave equation until using a slope-limit scheme.
time = 0; tstep = 0;

% Set timestep
k = CFL*h;

% integrate scheme
while (time<FinalTime)
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  u = u + k*LinwaveSLrhs1D(x,u,h,k,1);
  time = time+k; tstep = tstep+1;
end
return