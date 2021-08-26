function [q] = EulerSpec1D(x,q,N,L,CFL,gamma,FinalTime);
% function [q] = EulerSpec1D(x,q,N,L,CFL,gamma,FinalTime);
% Purpose  : Integrate 1D Euler equations until FinalTime using a Fourier
% spectral collocation method and 3rd order SSP-RK method
time = 0; tstep = 0; h = L/(2*N+1);

% Parameter for hyper viscosity
p = 8; 

% integrate scheme
while (time<FinalTime)
  % Decide on timestep
  pres = (gamma-1)*(q(:,3) - 0.5*q(:,2).^2./q(:,1)); 
  c = sqrt(gamma*pres./q(:,1)); maxvel = max(max(c+abs(q(:,2)./q(:,1))));
  k = CFL*h/maxvel;
  if (time+k>FinalTime) k = FinalTime-time; end
  
  % Update solution
  rhsq  = EulerSpecrhs1D(x,q,L,gamma); 
  q1 = q + k*rhsq;
  [q1(:,1)] = FourierVanishHypVisc(q1(:,1),p,k,N,L);
  [q1(:,2)] = FourierVanishHypVisc(q1(:,2),p,k,N,L);
  [q1(:,3)] = FourierVanishHypVisc(q1(:,3),p,k,N,L);
  rhsq  = EulerSpecrhs1D(x,q1,L,gamma); 
  q2 = (3*q + q1 + k*rhsq)/4;
  [q2(:,1)] = FourierVanishHypVisc(q2(:,1),p,k,N,L);
  [q2(:,2)] = FourierVanishHypVisc(q2(:,2),p,k,N,L);
  [q2(:,3)] = FourierVanishHypVisc(q2(:,3),p,k,N,L);  
  rhsu  = EulerSpecrhs1D(x,q2,L,gamma); 
  q = (q + 2*q2 + 2*k*rhsq)/3;
  [q(:,1)] = FourierVanishHypVisc(q(:,1),p,k,N,L);
  [q(:,2)] = FourierVanishHypVisc(q(:,2),p,k,N,L);
  [q(:,3)] = FourierVanishHypVisc(q(:,3),p,k,N,L);  
  time = time+k; tstep = tstep+1;
end
return