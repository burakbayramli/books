%multiple_try.m
T=10^3;M=100; % set up parameters
sigma=5; lam=12;
% define target pdf
f=@(x)exp(-(x(1)^2+x(2)^2+(x(1)*x(2))^2-2*lam*x(1)*x(2))/2);
X=[0,0]; % X_0
data=nan(T,2); count=0;
for t=1:T
    % step 1
    Y=repmat(X,M,1)+randn(M,2)*sigma;
    % step 2
    for i=1:M
        p(i)=f(Y(i,:));
    end
    Sum_p=sum(p);p=p/Sum_p;
    [dummy,J]=histc(rand,[0,cumsum(p)]);
    % step 3
    Z=repmat(Y(J,:),M-1,1)+randn(M-1,2)*sigma;
    Z(M,:)=X;
    % step 4
    for i=1:M
        w(i)=f(Z(i,:));
    end
    if rand<min(Sum_p/sum(w),1)
        X=Y(J,:); count=count+1;
    end
    data(t,:)=X;
end
count/T % acceptance rate estimate
plot(data(:,1),data(:,2),'.')
