function [a,f] = cavitybc(a,f,xy)
%CAVITYBC imposes essential boundary condition
%   [Agal,fgal] = cavitybc(A,f,xy);
%   input
%          A          stiffness matrix
%          f          rhs vector
%          xy         vertex coordinate vector
%   output
%          Agal       stiffness matrix
%          fgal       rhs vector
%   IFISS function: DJS; 3 December 2020.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
[bound,bc]=drivencavity_bc(xy);
ndof = length(f); nbd=length(bound);
null_col=sparse(ndof,nbd); null_row=sparse(nbd,ndof);
Ax=a(1:ndof,1:ndof);
fx=f(1:ndof);
%% set boundary condition
fx = fx - Ax(:,bound)*bc;
dA=zeros(ndof,1); dA(bound)=ones(nbd,1);
Ax(:,bound)=null_col;  Ax(bound,:)=null_row;   
Ax=Ax+spdiags(dA,0,ndof,ndof);  fx(bound)=bc;
a=Ax; f=fx;
return
