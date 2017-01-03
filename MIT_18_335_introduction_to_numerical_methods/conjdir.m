function xs=conjdir(A,b,x0)
%CONJDIR Conjugate Directions

r0=b-A*x0;
x=x0;
r=r0;
p0s=eye(size(A));

ps=[];
xs=x;
i=0;
while norm(r)>norm(r0)*1e-3
  i=i+1;

  p0=p0s(:,i);
  p=p0;
  for k=1:i-1
    pk=ps(:,k);
    beta=-(p0'*A*pk)/(pk'*A*pk);
    p=p+beta*pk;
  end
  
  Ap=A*p;
  alpha=(p'*r)/(p'*Ap);
  x=x+alpha*p;
  r=r-alpha*Ap;
 
  xs=[xs,x];
  ps=[ps,p];
end
