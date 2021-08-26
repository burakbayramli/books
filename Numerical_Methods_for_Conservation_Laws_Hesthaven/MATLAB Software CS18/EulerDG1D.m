function [q] = EulerDG1D(x,q,h,m,N,CFL,gamma,FinalTime)
% function [q] = EulerDG1D(x,q,h,m,N,CFL,gamma,FinalTime)
% Purpose  : Integrate 1D Euler equation until FinalTime using a DG
%            scheme and 3rd order SSP-RK method
% Initialize operators at Legendre Gauss Lobatto grid
r = LegendreGL(m); V  = VandermondeDG(m, r); D = DmatrixDG(m, r, V);
Ma = inv(V*V'); S = Ma*D; iV = inv(V);

% Compute operator for WENO smoothness evaluator
[qW,Xm,Xp] = WENODGWeights(m,iV);

% Initialize extraction vector
VtoE = zeros(2,N);
for j=1:N
  VtoE(1,j) = (j-1)*(m+1)+1; VtoE(2,j) = j*(m+1);
end

% Compute smallest spatial scale timestep
rLGLmin = abs(r(1)-r(2)); 
time = 0; tstep = 0; 

% integrate scheme
while (time<FinalTime)
  % Set timestep
  p = (gamma-1)*(q(:,:,3) - 0.5*q(:,:,2).^2./q(:,:,1)); 
  c = sqrt(gamma*p./q(:,:,1)); 
  maxvel = max(max(c+abs(q(:,:,2)./q(:,:,1)))); k = CFL*h*rLGLmin/maxvel;
  if (time+k>FinalTime) k = FinalTime-time; end
  
  % Stage 1 of SSPRK
  rhsq  = EulerDGrhs1D(x,q,h,k,m,N,gamma,S,Ma,VtoE,maxvel); 
  q1 = q + k*rhsq;
  
  % Limit solution through characteristics
  [qc,R] = EulerQtoRDG(q1,gamma,V,iV);
  R(:,:,1) = SlopeLimitCSDG(x,R(:,:,1),m,h,N,V,iV);
  R(:,:,2) = SlopeLimitCSDG(x,R(:,:,2),m,h,N,V,iV);
  R(:,:,3) = SlopeLimitCSDG(x,R(:,:,3),m,h,N,V,iV);
  q1 = EulerRtoQDG(R,qc,gamma,V,iV);
  
  % Stage 2 of SSPRK
  rhsq  = EulerDGrhs1D(x,q1,h,k,m,N,gamma,S,Ma,VtoE,maxvel); 
  q2 = (3*q + q1 + k*rhsq)/4;

  % Limit solution through characteristics
  [qc,R] = EulerQtoRDG(q2,gamma,V,iV);
  R(:,:,1) = SlopeLimitCSDG(x,R(:,:,1),m,h,N,V,iV);
  R(:,:,2) = SlopeLimitCSDG(x,R(:,:,2),m,h,N,V,iV);
  R(:,:,3) = SlopeLimitCSDG(x,R(:,:,3),m,h,N,V,iV);
  q2 = EulerRtoQDG(R,qc,gamma,V,iV);
  
  % Stage 3 of SSPRK
  rhsq  = EulerDGrhs1D(x,q2,h,k,m,N,gamma,S,Ma,VtoE,maxvel); 
  q = (q + 2*q2 + 2*k*rhsq)/3;

  % Limit solution through characteristics
  [qc,R] = EulerQtoRDG(q,gamma,V,iV);
  R(:,:,1) = SlopeLimitCSDG(x,R(:,:,1),m,h,N,V,iV);
  R(:,:,2) = SlopeLimitCSDG(x,R(:,:,2),m,h,N,V,iV);
  R(:,:,3) = SlopeLimitCSDG(x,R(:,:,3),m,h,N,V,iV);
  q = EulerRtoQDG(R,qc,gamma,V,iV);
 
  time = time+k; tstep = tstep+1; 
end
return