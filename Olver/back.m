function x = back(M)
%
%   x = back(M)
%    
%      Performs back substitution on augmented matrix M.
%      M is assumed to be in upper triangular form.
%

[m,n] = size(M);
if n ~= m+1 error('Wrong size!'); return; end

x(m,1) = M(m,m+1)/M(m,m);

for i= m-1:-1:1
   x(i,1) = (M(i,m+1) - M(i,i+1:m)*x(i+1:m,1))/M(i,i);
end
