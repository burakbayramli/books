function r=crosscovar(x,y,N)
% r=crosscovar(x,y,N)
%
% r = N x 1 cross-covariance vector of x(1:K) and y(1:K).
%
% r(n)=sum from k=1 to K-n+1 {x(k)*y(k+n-1)}; n=1:N.
% See also: crosscorr, crosscovar, autocovar


% Check for errors.
K=min(length(x),length(y));
if(nargin~=3)
   error('Function crosscovar must have 3 arguments.');
elseif(N>K),
   error('Third argument (N) cannot exceed min. vector length');
elseif(K<4)
   error('Signal vectors x and y must have at least 4 elements.');
end
% Compute r.
x=(row_vec(x));
y=row_vec(y);
r=zeros(N,1);
for n=1:N,
   r(n)=x(1:K-n+1)*y(n:K)';
end