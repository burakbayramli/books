function xs=conjgrad(A,b,x0)
%CONJGRAD Conjugate Gradient (educational)

r0=b-A*x0;
x=x0;
r=r0;

ps=[];
xs=x;
i=0;
while norm(r)>norm(r0)*1e-3
  i=i+1;

  p0=r;
  p=p0;

  if i>1
    pk=rold;
    beta=-(p0'*A*pk)/(pk'*A*pk);
    p=p+beta*pk;
  end
  
  Ap=A*p;
  alpha=(p'*r)/(p'*Ap);
  x=x+alpha*p;
  rold=r;
  r=r-alpha*Ap;
 
  xs=[xs,x];
  ps=[ps,p];
end
