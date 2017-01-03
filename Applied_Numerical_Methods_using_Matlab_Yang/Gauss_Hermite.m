function I=Gauss_Hermite(f,N,varargin)
[t,w]=???????(N);
ft=feval(f,t,varargin{:}); 
I= w*ft';
