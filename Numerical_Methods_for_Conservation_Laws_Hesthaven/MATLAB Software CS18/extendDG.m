function [ue] = extendDG(u,BCl,ul,BCr,ur)
% Purpose: Extend dependent and independent vectors u with m+1 entries
% subject to approproate boundary conditions for DG formulation
% BC = "D" - Dirichlet; BC = "N" - Neumann; BC = "P" - periodic
% u - BC value - only active for Dirichlet BC

dim = size(u); m = dim(1)-1; N = dim(2); 
ue = zeros(m+1,N+2); ue(:,2:N+1) = u;

% Periodic extension of u
if (BCl=='P') | (BCr=='P')  
   ue(:,1) = u(:,N); ue(:,N+2) = u(:,1);
   return;
end;

% Left extension
if BCl=='D'
   ue(:,1) = -flipud(u(:,1))+2*ul;
else
   ue(:,1) = flipud(u(:,1));        
end

% Right extension
if BCr=='D'
    ue(:,N+2) = -flipud(u(:,N))+2*ur; 
else 
    ue(:,N+2) = flipud(u(:,N));        
end
return