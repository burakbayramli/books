function x=cg(A,b)

x=0*b;
r=b;
p=b;

normr0=norm(r);
while norm(r)>1e-3*normr0
  Ap=A*p;
  rdotr=r'*r;

  alpha=rdotr/(p'*Ap);
  x=x+alpha*p;
  r=r-alpha*Ap;
  beta=(r'*r)/rdotr;
  p=r+beta*p;
end
