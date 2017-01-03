function [r,p,k]=part_frac_exp(b,a)
% [r,p,k]=part_frac_exp(b,a)
%
% Partial-fraction expansion of transfer function H(z).
% b and a are row-vector coefficients in
%        b(1)+b(2)*z^(-1)+...+b(N-1)*z^(-(N-1))
% H(z) = ------------------------------------ .
%        a(1)+a(2)*z^(-1)+...+a(M)*z^(-(M-1))
%
% r, p, and k are the coefficients in the expansion
%        r(1)*p(1)*z         r(M-1)*p(M-1)*z
% H(z)=-[----------- + ... + ---------------] + k(z^(-1)),
%         z-p(1)               z-p(M-1)
%      where k(z^(-1))=k(1)+k(2)z^(-1)+...+k(N-M+1)z^(M-N).
%      Note: k=0 if M>N; otherwise length(k)=N-M+1.
%
% If H(z) has a pole p(i) of multiplicity Q, terms of the form
%       p(i)*z
% r(i)*[------]^m, with m=2,3,...,Q are included inside the
%       z-p(i)
%       square brackets in the above expression for H(z).

% Initialize and check for errors.
b=row_vec(b);
a=row_vec(a);
N=length(b);
M=length(a);
if(N<=1|M<=1)
   error('b and a must each have >1 element.');
elseif(a(M)==0)
   error('Last element of a can''t be 0.');
elseif(b(1)==0 | a(1)==0)
   error('First elements of b and a must be nonzero in this version.');
end
% Find the residues of H(z^(-1)). Convert a to p & reverse kr.
br=b(N:-1:1)/a(M);
ar=a(M:-1:1)/a(M);
[r,a,kr]=residue(br,ar);
p=1./a;
k=kr(N-M+1:-1:1);