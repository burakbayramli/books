function [u] = BurgersDG1D(x,u,h,m,N,CFL,FinalTime)
% function [u] = BurgersDG1D(x,u,h,m,N,CFL,FinalTime)
% Purpose  : Integrate 1D Burgers equation until FinalTime using a DG
%            scheme and 3rd order SSP-RK method
% Initialize operators at Legendre Gauss Lobatto grid
r = LegendreGL(m); V  = VandermondeDG(m, r); D = DmatrixDG(m, r, V);
Ma = inv(V*V'); S = Ma*D; iV = inv(V);

% Compute operator for WENO smoothness evaluator
[Q,Xm,Xp] = WENODGWeights(m,iV);

% Initialize extraction vector
VtoE = zeros(2,N);
for j=1:N
  VtoE(1,j) = (j-1)*(m+1)+1; VtoE(2,j) = j*(m+1);
end

% Initialize filter matrix
% F = FilterDG(m,0,10,V);

% Compute smallest spatial scale timestep
rLGLmin = min(abs(r(1)-r(2))); 
time = 0; tstep = 0;

% Initialize parameters for nonlinear viscosity
nu = zeros(m+1,N); nu0 = 2; kappa = -6; c2 = 1;

% integrate scheme
while (time<FinalTime)
  % Decide on timestep
  maxvel = 2*max(max(abs(u))); k = CFL*rLGLmin*h/maxvel; 
  if (time+k>FinalTime) k = FinalTime-time; end  
  % Update solution - stage 1
  rhsu  = BurgersDGrhs1D(x,u,h,k,m,N,Ma,S,VtoE,maxvel); 
  u1 = u + k*rhsu;
  u1 = WENOlimitDG(x,u1,m,h,N,V,iV,Q,Xm,Xp);
  % Update solution - stage 2
  rhsu  = BurgersDGrhs1D(x,u1,h,k,m,N,Ma,S,VtoE,maxvel);
  u2 = (3*u + u1 + k*rhsu)/4;
  u2 = WENOlimitDG(x,u2,m,h,N,V,iV,Q,Xm,Xp);
  % Update solution - stage 3
  rhsu  = BurgersDGrhs1D(x,u2,h,k,m,N,Ma,S,VtoE,maxvel); 
  u = (u + 2*u2 + 2*k*rhsu)/3;
  u = WENOlimitDG(x,u,m,h,N,V,iV,Q,Xm,Xp);
  time = time+k; tstep = tstep+1;
end
return