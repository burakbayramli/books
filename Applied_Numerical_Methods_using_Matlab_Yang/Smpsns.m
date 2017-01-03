function INTf=smpsns(f, a,b, N,varargin)
%integral of f(x) over [a,b] by Simpson rule with N segments 
EPS=1e-12;
if nargin<4, N=100; end
if abs(b-a)<1e-12|N<=0, INTf=0; return; end
if mod(N,2)~=0, N=N+1; end %make N even
fa=feval(f,a,varargin{:}); 
if isnan(fa)|abs(fa)==inf, a=a+max(abs(a)*EPS,EPS); end
fb=feval(f,b,varargin{:}); 
if isnan(fb)|abs(fb)==inf, b=b-max(abs(b)*EPS,EPS); end
h=(b-a)/N;  x=a+[0:N]*h; %nodes(sample points)
kodd=2:2:N; keven=3:2:N-1; %the set of odd/even indices
fx=feval(f,x,varargin{:});
fx(find(fx==inf))=realmax; fx(find(fx==-inf))=-realmax; 
INTf= h/3*(fx(1)+fx(N+1)+4*sum(fx(kodd))+2*sum(fx(keven)));