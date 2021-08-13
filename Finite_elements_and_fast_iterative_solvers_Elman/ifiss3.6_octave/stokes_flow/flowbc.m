function [az,bz,fz,gz] = flowbc(a,b,f,g,xy,bound)
%FLOWBC imposes inflow boundary condition 
%   [Ast,Bst,fst,gst] = flowbc(A,B,f,g,xy,bound);
%   input
%          A          vector diffusion matrix
%          B          divergence matrix
%          f          velocity rhs vector
%          g          pressure rhs vector  
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%   output
%          Ast        vector diffusion matrix
%          Bst        divergence matrix
%          fst        velocity rhs vector
%          gst        pressure rhs vector  
%
%   calls function specific_flow
%   IFISS function: DJS, HCE; 30 August 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nu = length(f); np = length(g);
nvtx = nu/2; nbd=length(bound);
null_col=sparse(nvtx,nbd); %null_row=sparse(nbd,nvtx);
null_pcol=sparse(np,nbd);
Ax=a(1:nvtx,1:nvtx); Ay=a(nvtx+1:nu,nvtx+1:nu);
Bx=b(1:np,1:nvtx); By=b(1:np,nvtx+1:nu);
fx=f(1:nvtx); fy=f(nvtx+1:nu); gz=g;

%% set boundary condition
xbd=xy(bound,1); ybd=xy(bound,2);
[bcx,bcy]=specific_flow(xbd,ybd);

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
