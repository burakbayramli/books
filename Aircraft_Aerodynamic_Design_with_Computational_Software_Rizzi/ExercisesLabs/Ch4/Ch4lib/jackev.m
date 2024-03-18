function jac = jackev(fun,u0)
epsilon=1e-8; dum=0;
n=length(u0);
jac=zeros(n,n);
f0=feval(fun,dum,u0);
for i=1:n
   u=u0;
    u(i)=(1+epsilon)*u0(i);
    delta=u(i)-u0(i);
    jac(:,i)=(feval(fun,dum,u)-f0)/delta;
end
