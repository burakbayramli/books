function [q] = EulerC2D(x,y,q,hx,hy,gamma,CFL,FinalTime)
% function [q] = EulerC2D(x,y,q,hx,hy,gamma,CFL,FinalTime)
% Purpose  : Integrate 2D Euler equation until FinalTime using second order central scheme
time = 0; tstep = 0;

while (time<FinalTime)
  % Set timestep
  r = q(:,:,1); ru = q(:,:,2); rv = q(:,:,3); E = q(:,:,4);
  p = (gamma-1)*(E - 0.5*(ru.^2 + rv.^2)./r); c = sqrt(gamma*p./r); 
  maxvelu = max(max(c+abs(ru./r))); maxvelv = max(max(c+abs(rv./r)));
  k = CFL*min(hx,hy)/sqrt(2)/sqrt(maxvelu^2+maxvelv^2);
  if (time+k>FinalTime) k = FinalTime-time; end
  % Update solution
  [dq] = EulerCrhs2D(x,y,q,gamma,hx,hy,k);
  q = q + k*dq;
  time = time+k; tstep = tstep+1;
  time
end
return