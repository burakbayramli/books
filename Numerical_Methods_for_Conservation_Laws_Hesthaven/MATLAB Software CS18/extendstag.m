function [xe,ue] = extendstag(x,u,h,m,BCl,ul,BCr,ur)
% Purpose: Extend dependent and independent vectors (x,u), by m cells 
% subject to approproate boundary conditions. Grid is assumed staggered
% BC = "D" - Dirichlet; BC = "N" - Neumann; BC = "P" - periodic
% ul/ur - BC value - only active for Dirichlet BC

xl = min(x); xr = max(x); N = length(u);
xe = zeros(N+2*m,1); ue = zeros(N+2*m,1); q = [1:m];

% Extend x
xe(m-q+1) = xl-q*h; xe(N+m+q) = xr + q*h; xe((m+1):(N+m)) = x(1:N); 

% Periodic extension of u
if (BCl=='P') | (BCr=='P')  
  ue(m-q+1) = u(N-q+1); ue(N+m+q) = u(q); ue((m+1):(N+m)) = u(1:N);
  return;
end;

% Left extension
if BCl=='D'
    ue(m-q+1) = -u(q)+2*ul;    
else
    ue(m-q+1) = u(q);        
end

% Right extension
if BCr=='D'
    ue(N+m+q) = -u(N-q+1)+2*ur; 
else 
    ue(N+m+q) = u(N-q+1);        
end
ue((m+1):(N+m)) = u(1:N);
return