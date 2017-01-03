function INTf=trpzds(f,a,b,N)
%integral of f(x) over [a,b] by trapezoidal rule with N segments
if abs(b-a)<eps|N<=0, INTf=0; return; end
h=(b-a)/N; x=a+[0:N]*h; 
fx=feval(f,x); values of f for all nodes
INTf= h*((fx(1)+fx(N+1))/2+sum(fx(2:N))); %Eq.(5.6-1)
