function [g,b]=rls_filter(b,u,alpha,d,f)
% [g,b]=rls_filter(b,u,alpha,d,f)
%
% Inputs:
%   b  = initial weight vector b(0), e.g., zeros(10,1).
%   u  = adaptive gain constant; 0<=u<<1.
%   alpha = decay factor in power estimates; 0<alpha<<1.
%   d  = desired signal vector.
%   f  = input vector to FIR adaptive filter.
% Output:  
%   g  = adaptive filter output vector; error=d-g.
%   b  = successive weight vectors, [b(0),...,b(K-1)]
%
% This is a version of the adaptive RLS algorithm
% computed as in (9.68) in the text.
% Initial power =f'*f/length(f).
% See also: lms_filter
f=row_vec(f)';
d=row_vec(d)';
K=min(length(d),length(f));
b=row_vec(b)';
N=length(b);
% Check for errors.
if(u<0 | u>=1 | alpha<=0 | alpha>=1),
   error('Adaptive parameters u and/or alpha out of range.');
elseif(K<10)
	error('Lengths of signals f and d should be >9.');
end
% Initialize g(1xK) and b(NxK) and begin f with N-1 zeros.
g=zeros(K,1);
b=[b,zeros(N,K-1)];
f=[zeros(N-1,1); f];
% Initialize s2=sigma sq., and initialize Phiffinv.
s2=f'*f/K;
Phiffinv=diag(ones(1,N))/s2;
% Filter; update g and next col. of b at each step.
for k=1:K,
	fvect=f(k+N-1:-1:k);
	S=Phiffinv*fvect;
	denom=1-alpha+alpha*fvect'*S;
	Phiffinv=(Phiffinv-alpha*S*S'/denom)/(1-alpha);
	g(k)=b(:,k)'*fvect;
	e=d(k)-g(k);
	b(:,min(k+1,K))=b(:,k)+2*u*Phiffinv*e*fvect;
end