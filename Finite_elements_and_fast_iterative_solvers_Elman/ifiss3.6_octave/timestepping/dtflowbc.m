function [az,bz,fz,gz] = dtflowbc(a,b,f,g,xy,bound,told,t)
%DTFLOWBC imposes updated BC at the current timestep
%   [Ast,Bst,fst,gst] = dtflowbc(A,B,f,g,xy,bound,told,tcurrent);
%   input
%          A          vector diffusion matrix
%          B          divergence matrix
%          f          velocity rhs vector
%          g          pressure rhs vector  
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%          told       previous time
%          tcurrent   current time
%   output
%          Ast        vector diffusion matrix
%          Bst        divergence matrix
%          fst        velocity rhs vector
%          gst        pressure rhs vector  
%
%   calls function specific_flow
%   IFISS function: DJS; 24 November 2009.
% Copyright (c) 2009 D.J. Silvester
tout=0; %ouput parameter
nu = length(f); np = length(g);
nvtx = nu/2; nbd=length(bound);
null_col=sparse(nvtx,nbd); %null_row=sparse(nbd,nvtx);
null_pcol=sparse(np,nbd);
Ax=a(1:nvtx,1:nvtx); Ay=a(nvtx+1:nu,nvtx+1:nu);
Bx=b(1:np,1:nvtx); By=b(1:np,nvtx+1:nu);
fx=f(1:nvtx); fy=f(nvtx+1:nu); gz=g;

%% get boundary condition at current and previous times
xbd=xy(bound,1); ybd=xy(bound,2);
[bcx,bcy]=specific_flow(xbd,ybd);
bcxold=bcx*(1-exp(-10*told)); bcyold=bcy*(1-exp(-10*told));
bcxnew=bcx*(1-exp(-10*t));    bcynew=bcy*(1-exp(-10*t));
dt=t-told;
if tout==1,
fprintf('updated boundary values: timestep is %e\n',dt)
end
bcx=(bcxnew-bcxold)/dt; bcy=(bcynew-bcyold)/dt;
if tout==1,
fprintf('     boundary change is %e \n',dt*norm([bcx;bcy]))
end

%% impose boundary condition
fx = fx - Ax(:,bound)*bcx;  fy = fy - Ay(:,bound)*bcy;  
gz = gz - Bx(:,bound)*bcx;  gz = gz - By(:,bound)*bcy;
dA=zeros(nvtx,1); dA(bound)=ones(nbd,1);

%Procedure (for Ax and Ay) is equivalent to commented lines
%It is more efficient because only columns are referenced

%Ax(:,bound)=null_col;  Ax(bound,:)=null_row;   
Axt = Ax'; Axt(:,bound) = null_col;
Ax = Axt'; Ax(:,bound) = null_col;
Ax=Ax+spdiags(dA,0,nvtx,nvtx);  fx(bound)=bcx; 

%Ay(:,bound)=null_col;  Ay(bound,:)=null_row;   
Ayt = Ay'; Ayt(:,bound) = null_col;
Ay = Ayt'; Ay(:,bound) = null_col;
Ay=Ay+spdiags(dA,0,nvtx,nvtx);  fy(bound)=bcy; 

Bx(:,bound)=null_pcol; By(:,bound)=null_pcol;

az=[Ax,sparse(nvtx,nvtx);sparse(nvtx,nvtx),Ay];
bz=[Bx,By]; fz=[fx;fy]; 
return
