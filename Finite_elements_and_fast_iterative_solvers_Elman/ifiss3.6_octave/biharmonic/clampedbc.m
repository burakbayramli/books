function [a,f] = clampedbc(a,f,xy)
%CLAMPEDBC imposes essential boundary condition
%   [Agal,fgal] = clampedbc(A,f,xy);
%   input
%          A          stiffness matrix
%          f          rhs vector
%          xy         vertex coordinate vector
%   output
%          Agal       stiffness matrix
%          fgal       rhs vector
%   IFISS function: DJS; 1 August 2021.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
weak=default('weak/strong enforcement of tangential BC (1/0)? (default weak)',1);
if weak, [bound,bc]=plate_bcx(xy);
else [bound,bc]=plate_bc(xy); end
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
