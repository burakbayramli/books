function [h,fx] = dtheatbc(a,f,xy,bound,told,t)
%DTHEATBC imposes Dirichlet BC at the current timestep
%   [Hgal,fgal] = dtheatbc(A,f,xy,bound,told,tcurrent); 
%   input
%          A          reaction-diffusion matrix without bc's
%          f          rhs vector without bc's
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%          told       previous time
%          t          current time   
%   output
%          Hgal        reaction-diffusion matrix with bc's
%          fgal        updated rhs vector
%
%   calls function specific_bc
%   IFISS function: DJS; 6 December 2009.
% Copyright (c) 2009 D.J. Silvester 
tout=0; %ouput parameter
nvtx = length(f); nbd=length(bound);
null_col=sparse(nvtx,nbd); null_row=sparse(nbd,nvtx);
Ax=a(1:nvtx,1:nvtx); fx=f(1:nvtx);

%% get boundary condition at current and previous times
xbd=xy(bound,1); ybd=xy(bound,2);
bc=specific_bc(xbd,ybd);
bcold=bc*(1-exp(-10*told)); 
bcnew=bc*(1-exp(-10*t));   
dt=t-told;
if tout==1,
fprintf('updated boundary values: timestep is %e\n',dt)
end
bc=2*(bcnew-bcold)/dt; 
if tout==1,
fprintf('     boundary change is %e \n',dt*norm(bc))
end

%% impose boundary condition
fx = f - a(:,bound)*bc; 
fx(bound)=bc;
dA=zeros(nvtx,1); dA(bound)=ones(nbd,1);
Ax(:,bound)=null_col;  Ax(bound,:)=null_row;   
Ax=Ax+spdiags(dA,0,nvtx,nvtx);  
h=Ax; 
return
