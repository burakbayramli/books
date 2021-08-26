function [q] = EulerM1D(x,q,h,CFL,gamma,FinalTime)
% function [q] = EulerMDriver1D(x,q,CFL,gamma,FinalTime)
% Purpose: Integrate the 1D Euler equations until FinalTime 
% using a monotone scheme.
time = 0; tstep = 0; 

% integrate scheme
while (time<FinalTime)
  % Set timestep
  p = (gamma-1)*(q(:,3) - 0.5*q(:,2).^2./q(:,1)); c = sqrt(gamma*p./q(:,1)); 
  maxvel = max(c+abs(q(:,2)./q(:,1))); k = CFL*h/maxvel;
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  q = q + k*EulerMrhs1D(x,q,gamma,h,k,maxvel);
  time = time+k; tstep = tstep+1;
end
return