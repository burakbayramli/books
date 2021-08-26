function [q] = EulerWENO2D(x,y,q,hx,hy,m,gamma,CFL,FinalTime)
% function [q] = EulerWENO2D(x,y,q,hx,hy,m,gamma,CFL,FinalTime)
% Purpose  : Integrate 2D Euler equation until FinalTime using a WENO scheme.

time = 0; tstep = 0;

% Initialize reconstruction weights
Crec = zeros(m+1,m);
for rr=-1:m-1;
    Crec(rr+2,:) = ReconstructWeights(m,rr);
end;

% Initialize linear weights
dw = LinearWeights(m,0);

% Compute smoothness indicator matrices
beta = zeros(m,m,m);
for rr=0:m-1
    xl = -1/2 + [-rr:1:m-rr];
    beta(:,:,rr+1) = betarcalc(xl,m);
end

while (time<FinalTime)
  % Set timestep
  r = q(:,:,1); ru = q(:,:,2); rv = q(:,:,3); E = q(:,:,4);
  p = (gamma-1)*(E - 0.5*(ru.^2 + rv.^2)./r); c = sqrt(gamma*p./r); 
  maxvelu = max(max(c+abs(ru./r))); maxvelv = max(max(c+abs(rv./r)));
  k = CFL*min(hx,hy)/sqrt(2)/sqrt(maxvelu^2+maxvelv^2);
  if (time+k>FinalTime) k = FinalTime-time; end
  
  % Update solution
  [rhsq]  = EulerWENOrhs2D(x,y,q,hx,hy,k,m,Crec,dw,beta,gamma);
  q1 = q + k*rhsq; 
  [rhsq]  = EulerWENOrhs2D(x,y,q1,hx,hy,k,m,Crec,dw,beta,gamma);
  q2 = (3*q + q1 + k*rhsq)/4; 
  [rhsq]  = EulerWENOrhs2D(x,y,q2,hx,hy,k,m,Crec,dw,beta,gamma);
  q = (q + 2*q2 + 2*k*rhsq)/3; 
  time = time+k; tstep = tstep+1;
end
return