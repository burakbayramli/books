function R=autocovar_mat(x,N)
% R=autocovar_mat(x,N)
%
% R = N x N autocovariance matrix of x(1:K)
%
% R(m,n)=sum from k=1 to K-m+1 {x(k)*x(k+m-n)}; m=1:N and m>=n.
%       =R(n,m); m<n.
%
% (See text for derivation.)

% Check for errors.
K=length(x);
if(nargin~=2)
   error('Function autocovar_mat must have two arguments.');
elseif(N>K),
   error('Third argument (N) cannot exceed length(x)');
elseif(K<4)
   error('Signal vector must have at least 4 elements.');
end
% Compute R.
x=(row_vec(x))';
R=zeros(N,N);
for m=1:N,
   for n=1:m,
      R(m,n)=x(1:K-m+1)'*x(m-n+1:K-n+1);
      R(n,m)=R(m,n);
   end
end