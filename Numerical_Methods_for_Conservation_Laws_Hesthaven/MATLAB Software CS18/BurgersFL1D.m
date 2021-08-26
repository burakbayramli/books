function [u] = BurgersFL1D(x,u,h,CFL,FinalTime)
% function [u] = BurgersLF1D(x,u,h,CFL,FinalTime)
% Purpose  : Integrate 1D Burgers equation until FinalTime using a flux limited scheme.
time = 0; tstep = 0;

% integrate scheme
while (time<FinalTime)
  % Decide on timestep
  maxvel = max(2*abs(u)); k = CFL*h/maxvel;
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  u = u + k*BurgersFLrhs1D(x,u,h,k,maxvel);
  time = time+k; tstep = tstep+1;
end
return