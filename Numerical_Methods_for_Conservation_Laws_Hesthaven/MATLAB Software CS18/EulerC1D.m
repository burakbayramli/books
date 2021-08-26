function [q] = EulerC1D(x,q,h,CFL,gamma,FinalTime)
% function [q] = EulerC1D(x,q,h,CFL,gamma,FinalTime)
% Purpose  : Integrate 1D Euler equation until FinalTime using a 
% second order central scheme.
time = 0; tstep = 0; 

% integrate scheme
while (time<FinalTime)
  % Set timestep
  p = (gamma-1)*(q(:,3) - 0.5*q(:,2).^2./q(:,1)); c = sqrt(gamma*p./q(:,1)); 
  maxvel = max(c+abs(q(:,2)./q(:,1))); k = CFL*h/maxvel;
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  q = q + k*EulerCrhs1D(x,q,gamma,h,k,maxvel); 
  time = time+k; tstep = tstep+1; 
end
return