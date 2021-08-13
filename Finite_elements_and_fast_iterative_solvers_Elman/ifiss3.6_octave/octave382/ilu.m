function [L,U] = ilu(A,setup)
%ILU  incomplete ILU with no fill-in within octave  
%   [L,U] = ilu(A,setup);
%   input
%          A          coefficient matrix 
%          setup      factorization structure (not used)
%   output
%          L,U        sparse factors
%
% calls ilu0: but is slow compared to matlab ilu!
%   IFISS function: DJS; 15 October 2013.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
[nn,dummy]=size(A);
if nn==1, L(1,1)=1; U(1,1)=A(1,1); 
else [L,U]=ilu0(A); end
return
