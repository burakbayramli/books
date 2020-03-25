%independence_sampler.m
clear all, a=400; b=2; c=1;
p=@(x)sqrt((b*c)^2*sin(x(2)).^2.*cos(x(1)).^2+...
    (a*c)^2*sin(x(2)).^2.*sin(x(1)).^2+...
    (a*b)^2*cos(x(2)).^2);
%define alpha(x,y) in the Metropolis algorithm
alpha=@(x,y)min(1,sqrt  ( p(y) ./ p(x) ) );
X=ones(2,1)*pi/2; % initial starting point
T=10^4; data=nan(T,2); % preallocate memory
accept_prob=0;
for t=1:T
    Y=[rand*2*pi;acos(1-2*rand)]; % make proposal
    if rand<alpha(X,Y) % Metropolis criterion
        X=Y; accept_prob=accept_prob+1;
    end
    data(t,:)=X';
end
accept_prob=accept_prob/T
K=100; x=data(:,1); ell=mean(x);
for k=0:K
    R(k+1)= (x(1:end-k)-ell)'*(x(k+1:end)-ell);
    R(k+1)=R(k+1)/(length(x)-k-1);
end
plot([0:K],R)

