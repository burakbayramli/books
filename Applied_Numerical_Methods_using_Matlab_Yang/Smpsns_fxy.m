function INTf=smpsns_fxy(f, x, c, d, N)
%1-dimensional integration of f(x,y) for Ry={c<=y<=d}
if nargin<5, N=100; end
if abs(d-c)<eps|N<=0, INTf=0; return; end
if mod(N,2)~=0,  N=N+1; end
h=(d-c)/N; y=c+[0:N]*h; 
fxy=feval(f,x,y);
kodd=2:2:N; keven=3:2:N-1; %the set of odd/even indices
INTf= h/3*(fxy(1)+fxy(N+1)+4*sum(fxy(kodd))+2*sum(fxy(keven)));  
%To avoid the vector operation
%sum_odd=f(x,y(2));  sum_even=0;
%for n=4:2:N
%   sum_odd=sum_odd+f(x,y(n)); sum_even=sum_even+f(x,y(n-1));
%end
%INTf=(f(x,y(1))+f(x,y(N+1))+4*sum_odd+2*sum_even)*h/3;