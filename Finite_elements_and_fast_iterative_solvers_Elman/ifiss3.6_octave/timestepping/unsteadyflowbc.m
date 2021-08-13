function [fz,gz] = unsteadyflowbc(a,b,f,g,xy,bound,t)
%UNSTEADYFLOWBC imposes inflow boundary condition 
%   [fst,gst] = unsteadyflowbc(A,B,f,g,xy,bound,t);
%   input
%          A          vector diffusion matrix without bc's
%          B          divergence matrix
%          f          velocity rhs vector
%          g          pressure rhs vector  
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%          t          current time
%   output
%          fst        updated velocity rhs vector
%          gst        updated pressure rhs vector  
%
%   calls function specific_flow
%   IFISS function: DJS; 19 November 2009.
% Copyright (c) 2006 D.J. Silvester, D.F. Griffiths, M. Schneider  
nu = length(f); np = length(g);
nvtx = nu/2; nbd=length(bound);
Ax=a(1:nvtx,1:nvtx); Ay=a(nvtx+1:nu,nvtx+1:nu);
Bx=b(1:np,1:nvtx); By=b(1:np,nvtx+1:nu);
fx=f(1:nvtx); fy=f(nvtx+1:nu); gz=g;

%% set boundary condition
xbd=xy(bound,1); ybd=xy(bound,2);
[bcx,bcy]=specific_flow(xbd,ybd);
% smoothly grow the hot wall profile
bcx=bcx*(1-exp(-10*t)); bcy=bcy*(1-exp(-10*t));
%% and adjust the RHS vectors accordingly 
fx = fx - Ax(:,bound)*bcx;  fy = fy - Ay(:,bound)*bcy;  
gz = gz - Bx(:,bound)*bcx;  gz = gz - By(:,bound)*bcy;
fx(bound)=bcx;  fy(bound)=bcy; 
fz=[fx;fy]; 
return
