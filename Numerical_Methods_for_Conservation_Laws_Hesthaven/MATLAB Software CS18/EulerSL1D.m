function [q] = EulerSL1D(x,q,h,CFL,gamma,FinalTime)
% function [q] = EulerFL1D(x,q,CFL,gamma,FinalTime)
% Purpose  : Integrate 1D Euler equation until FinalTime using a 
% slope limited scheme and SSP-RK3.
time = 0; tstep = 0;

% integrate scheme
while (time<FinalTime)
  % Set timestep
  p = (gamma-1)*(q(:,3) - 0.5*q(:,2).^2./q(:,1)); c = sqrt(gamma*p./q(:,1)); 
  maxvel = max(c+abs(q(:,2)./q(:,1))); k = CFL*h/maxvel;
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  rhsq  = EulerSLrhs1D(x,q,gamma,h,k,maxvel);  q1 = q + k*rhsq;
  rhsq  = EulerSLrhs1D(x,q1,gamma,h,k,maxvel); q2 = (3*q + q1 + k*rhsq)/4;
  rhsq  = EulerSLrhs1D(x,q2,gamma,h,k,maxvel); q = (q + 2*q2 + 2*k*rhsq)/3;
  time = time+k; tstep = tstep+1; 
end
return