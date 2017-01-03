function [LAMBDA,V,ermsg]=eig_Jacobi(A,EPS,MaxIter)
%Jacobi method finds the eigenvalues/eigenvectors of symmetric matrix A
if nargin<3, MaxIter=100; end
if nargin<2, EPS=1e-8; end
N= size(A,2); 
LAMBDA=[]; V=[];
for m=1:N
   if norm(A(m:N,m)-A(m,m:N)')>EPS
 error('asymmetric matrix!'); 
end
end
V= eye(N); 
for k=1:MaxIter
  for m=1:N-1
     [Am(m),Q(m)]= max(abs(A(m,m+1:N))); 
  end
  [Amm,p]= max(Am);  q=p+Q(p);
  if Amm<EPS*sum(abs(diag(LAMBDA)))
     break;
  end
  if abs(A(p,p)-A(q,q))<EPS
     s2= 1;  s= 1/sqrt(2); c=s;
     cc=c*c; ss=s*s;
  else
     t2= 2*A(p,q)/(A(p,p)-A(q,q));   %Eq.(8.4-9a)
     c2= 1/sqrt(1+t2*t2); s2=t2*c2;  %Eq.(8.4-9b,c)
     c= sqrt((1+c2)/2); s=s2/2/c;    %Eq.(8.4-9d,e)
     cc=c*c; ss=s*s;
  end
  LAMBDA= A;
  LAMBDA(p,:)= A(p,:)*c +A(q,:)*s;   %Eq.(8.4-7b)
  LAMBDA(:,p)= LAMBDA(p,:)';
  LAMBDA(q,:)=-A(p,:)*s +A(q,:)*c;   %Eq.(8.4-7c)
  LAMBDA(:,q)= LAMBDA(q,:)';
  LAMBDA(p,q)= 0;  LAMBDA(q,p)= 0;   %Eq.(8.4-7a)
  LAMBDA(p,p)= A(p,p)*cc +A(q,q)*ss +A(p,q)*s2; %Eq.(8.4-7d)
  LAMBDA(q,q)= A(p,p)*ss +A(q,q)*cc -A(p,q)*s2; %Eq.(8.4-7e)
  A= LAMBDA;
  V(:,[p q])= V(:,[p q])*[c -s;s c];
end
LAMBDA= diag(diag(LAMBDA)); %for purification
