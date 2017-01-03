function y=zeroing(x,M,m)
%zero out every (kM-m)th element
if nargin<3, m=0; end
if M<=0, M=1; end
m=mod(m,M);
Nx=length(x); N=floor(Nx/M);
y=x; y(M*[1:N]-m)=0; 
