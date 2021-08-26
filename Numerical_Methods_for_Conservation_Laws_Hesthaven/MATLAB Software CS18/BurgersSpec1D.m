function [u] = BurgersSpec1D(x,u,N,L,CFL,FinalTime);
% function [u] = BurgersSpec1D(x,u,N,L,CFL,FinalTime);
% Purpose  : Integrate 1D Burgers equation until FinalTime using a Fourier
% spectral collocation method and 3rd order SSP-RK method
time = 0; tstep = 0; h = L/(2*N+1);

% Parameter for hyper viscosity
p = 16; 

% integrate scheme
while (time<FinalTime)
  % Decide on timestep
  maxvel = max(2*abs(u)); k = CFL*h/maxvel;
  if (time+k>FinalTime) k = FinalTime-time; end
  
  % Update solution
  rhsu  = BurgersSpecrhs1D(x,u,L); u1 = u + k*rhsu;
  [u1] = FourierVanishHypVisc(u1,p,k,N,L);
  rhsu  = BurgersSpecrhs1D(x,u1,L); u2 = (3*u + u1 + k*rhsu)/4;
  [u2] = FourierVanishHypVisc(u2,p,k,N,L);
  rhsu  = BurgersSpecrhs1D(x,u2,L); u = (u + 2*u2 + 2*k*rhsu)/3;
  [u] = FourierVanishHypVisc(u,p,k,N,L);
  time = time+k; tstep = tstep+1;
end
return