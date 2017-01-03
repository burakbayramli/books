%3.1  FDcode.m

n=8; h=1/(n+1); f=ones(n,1)-h*(1:n)';C=diag(f);
A=eye(n) - diag(ones,n-1,1),-1); K=A'*C*A/h^2;
xhalf=h*(1:n)'/2; uexact=xhalf- xhalf.^2; 
U=K\f
error=uexact-U
error./(1:n)'
