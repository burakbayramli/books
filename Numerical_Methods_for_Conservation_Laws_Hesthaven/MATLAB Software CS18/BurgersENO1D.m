function [u] = BurgersENO1D(x,u,h,m,CFL,FinalTime)
% function [u] = BurgersENO1D(x,u,h,m,r0,CFL,FinalTime)
% Purpose  : Integrate 1D Burgers equation until FinalTime using an ENO
%            scheme and 3rd order SSP-RK method
time = 0; tstep = 0;

% Initialize reconstruction weights
Crec = zeros(m+1,m);
for r=-1:m-1;
    Crec(r+2,:) = ReconstructWeights(m,r);
end;

% integrate scheme
while (time<FinalTime)
  % Decide on timestep
  maxvel = max(2*abs(u)); k = CFL*h/maxvel;
  if (time+k>FinalTime) k = FinalTime-time; end
  
  % Update solution
  rhsu  = BurgersENOrhs1D(x,u,h,k,m,Crec,maxvel); 
  u1 = u + k*rhsu;
  rhsu  = BurgersENOrhs1D(x,u1,h,k,m,Crec,maxvel); 
  u2 = (3*u + u1 + k*rhsu)/4;
  rhsu  = BurgersENOrhs1D(x,u2,h,k,m,Crec,maxvel); 
  u = (u + 2*u2 + 2*k*rhsu)/3;
  time = time+k; tstep = tstep+1;
end
return