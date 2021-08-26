function [u] = BurgersFL2D(x,y,u,hx,hy,CFL,FinalTime)
% function [u] = BurgersFL2D(u,hx,hy,CFL,FinalTime)
% Purpose  : Integrate 2D Burgers equation until FinalTime using a flux limited scheme.
time = 0; tstep = 0;

% integrate scheme
while (time<FinalTime)
  % Decide on timestep
  maxvel = max(max(2*sqrt(2.0)*abs(u))); k = CFL*min(hx,hy)/maxvel/2;
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  u = u + k*BurgersFLrhs2D(x,y,u,hx,hy,k,maxvel);
  time = time+k; tstep = tstep+1;
end
return