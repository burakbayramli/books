function [x,residuals2norm,errors2norm,errorsAnorm]=cg_stats(A,b,x0)

x=0*b;
r=b;
p=b;

residuals2norm=norm(r);
errors2norm=sqrt(x0'*x0);
errorsAnorm=sqrt(x0'*A*x0);
while residuals2norm(end)>1e-3*residuals2norm(1)
  Ap=A*p;
  rdotr=r'*r;

  alpha=rdotr/(p'*Ap);
  x=x+alpha*p;
  r=r-alpha*Ap;
  beta=(r'*r)/rdotr;
  p=r+beta*p;
  
  residuals2norm(end+1)=norm(r);
  errors2norm(end+1)=sqrt((x-x0)'*(x-x0));
  errorsAnorm(end+1)=sqrt((x-x0)'*A*(x-x0));
end
