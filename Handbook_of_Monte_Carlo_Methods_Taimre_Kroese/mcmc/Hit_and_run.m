%Hit_and_run.m
Sig=[1,0.9;0.9,1]; Mu=[1,1]'/2; B=chol(Sig)';
x=[5,5]';             %starting point
T=10^4; data=nan(T,2);% number of points desired
for t=1:T
    d=randn(2,1); d=d/norm(d);
    D=(d'*x)^2-x'*x+25;
    z1=B\d; z2=B\(x-Mu);
    % determine mean and variance of lambda dist.
    sig=1/norm(z1); mu=-z1'*z2*sig^2;
    if D<0
        lam=mu+randn*sig;
    else
        lam=normt2(mu,sig,-sqrt(D)-d'*x,sqrt(D)-d'*x);
    end
    x=x+lam*d;
    data(t,:)=x';
end
plot(data(:,1),data(:,2),'r.'),axis equal,
hold on
ezplot('x^2+y^2-25')

