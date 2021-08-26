function [q] = MaxwellDG1D(x,q,ep,mu,h,m,N,CFL,FinalTime)
% function [q] = MaxwellDG1D(x,q,ep,mu,h,m,N,CFL,FinalTime)
% Purpose  : Integrate 1D Maxwells equation until FinalTime using a DG
%            scheme and a 3rd order SSP-RK method.
% Initialize operators at Legendre Gauss Lobatto grid
r = LegendreGL(m); V  = VandermondeDG(m, r); D = DmatrixDG(m, r, V);
Ma = inv(V*V'); S = Ma*D;

% Initialize extraction vector
VtoE = zeros(2,N);
for j=1:N
  VtoE(1,j) = (j-1)*(m+1)+1; VtoE(2,j) = j*(m+1);
end
  
% Compute smallest spatial scale timestep
rLGLmin = min(abs(r(1)-r(2))); 
time = 0; tstep = 0;

% Set timestep
cvel = 1./sqrt(ep.*mu); maxvel = max(max(cvel));
k = CFL*rLGLmin*h/2/maxvel; 

% integrate scheme
while (time<FinalTime)
  if (time+k>FinalTime) k = FinalTime-time; end
  
  % Update solution
  [rhsq]  = MaxwellDGrhs1D(x,q,ep,mu,h,k,m,N,Ma,S,VtoE,maxvel); 
  q1 = q + k*rhsq; 
  [rhsq]  = MaxwellDGrhs1D(x,q1,ep,mu,h,k,m,N,Ma,S,VtoE,maxvel); 
  q2 = (3*q + q1 + k*rhsq)/4; 
  [rhsq]  = MaxwellDGrhs1D(x,q2,ep,mu,h,k,m,N,Ma,S,VtoE,maxvel); 
  q  = (q + 2*q2 + 2*k*rhsq)/3;
  time = time+k; tstep = tstep+1;
end
return