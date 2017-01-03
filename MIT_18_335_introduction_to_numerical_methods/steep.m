function xs=steep(A,b,x0)
%STEEP Steepest Descent

r0=b-A*x0;
x=x0;
r=r0;

xs=x;
while norm(r)>norm(r0)*1e-3
  Ar=A*r;
  alpha=(r'*r)/(r'*Ar);
  x=x+alpha*r;
  r=r-alpha*Ar;
  
  xs=[xs,x];
end
