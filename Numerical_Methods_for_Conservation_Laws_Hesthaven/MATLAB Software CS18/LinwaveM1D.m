function [u] = LinwaveM1D(x,u,h,CFL,FinalTime)
% function [u] = LinwaveM1D(x,u,h,CFL,FinalTime)
% Purpose  : Integrate 1D wave equation until using a monotone scheme.
time = 0; tstep = 0;

% Set the timestep
k = CFL*h;

% Integrate scheme
while (time<FinalTime)
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  u = u + k*LinwaveMrhs1D(x,u,h,k,1);
  time = time+k; tstep = tstep+1;
end
return