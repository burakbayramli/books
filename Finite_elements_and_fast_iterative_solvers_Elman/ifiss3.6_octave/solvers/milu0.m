function [L,U] = milu0(A)
%MILU0 modified incomplete factorization with no fill-in
%   [L,U] = milu0(A)
%   input
%          A            nonsingular sparse matrix A
%   output
%          L            lower triangular factor 
%          U            upper triangular factor
%
%   IFISS function: HCE, 2005; DJS; 6 January 2009
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage
%
% Warning: this program is not officially part of ifiss.
% It runs inefficiently with respect to CPU time because of its loop structure.
% Its primary use is to demonstrate the performance (in terms of iteration
% counts and matrix conditioning) of the milu(0) preconditioner, as indicated
% in Computational Exercise 2.4 of Elman, Silvester & Wathen,
% "Finite Elements and Fast Iterative Solvers".
%error('Defunct IFISS function: call "ilu" with setup.milu="row" instead!');
%%
L = 0*A;
U = 0*A;
N = length(A);

for i=1:N,
   j1 = min(find(A(i,:)));
   j2 = max(find(A(i,:)));   
   for j=j1:j2,
      m = min(i,j)-1;
      s = A(i,j) - L(i,1:m)*U(1:m,j);
      if A(i,j)~=0,
         if i>j,      L(i,j) = s;
         elseif i==j, L(i,i) = L(i,i) + s;
         else         U(i,j) = s;
         end
      else
         L(i,i) = L(i,i) + s;
      end
   end
   U(i,i+1:j2) = U(i,i+1:j2)/L(i,i);
   U(i,i) = 1;
end

if A==A', 
   D = sqrt(diag(L,0));
   Di = 1./D;
   L = L*spdiags(Di,0,N,N);
   U = spdiags(D,0,N,N)*U;
end
