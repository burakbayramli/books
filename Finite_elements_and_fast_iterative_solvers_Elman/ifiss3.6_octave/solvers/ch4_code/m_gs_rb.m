function w=m_gs_rb(v,aparams,mparams)
%m_gs_rb     line red-black Gauss-Seidel preconditioning
%   w = m_gs_rb(v,aparams,mparams)
%   input
%          v            operand for preconditioning operator
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix (not used)
%   output
%          w            result of preconditioning operation
%
%   IFISS function: HCE; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

%Inefficient:  recomputes preconditioner on each call

perm = [];

n = sqrt(size(aparams.A,1));
if mod(n,2)==1, maxred = n;   maxblk = n-1;
else            maxred = n-1; maxblk = n;
end

for i=1:2:maxred,
   perm = [perm; [(i-1)*n+1:i*n]'];
end
for i=2:2:maxblk,
   perm = [perm; [(i-1)*n+1:i*n]'];
end

Arb = aparams.A(perm,perm);
Q = tril(Arb,1);

vrb = v(perm);
wperm = Q\vrb;

w=zeros(length(wperm),1); 
w(perm) = wperm;