function I=Gauss_Legendre(f,a,b,N,varargin)
%Never try N larger than 25
[t,w]=Gausslp(N); 
x=((b-a)*t+a+b)/2;
fx=feval(f,x,varargin{:});
I= w*fx'*(b-a)/2;
