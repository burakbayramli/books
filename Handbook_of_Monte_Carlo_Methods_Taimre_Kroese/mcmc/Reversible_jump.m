%Reversible_jump.m
randn('state',4)
a=(0:100)'/20;
b=[1,0.3,0.15];
A=[a.^0,a,a.^2]; % coefficient matrix
z=A*b'+randn(101,1); % generate data
% posterior density
f=@(beta)exp(-0.5*norm(z-A(:,1:length(beta))*beta')^2);
% proposal
g=@(u)(exp(-0.5*norm(u)^2)/(2*pi)^(length(u)/2));
m=1;x=randn(1,m); T=10^5; % initialize
data=nan(T,1); beta0=0; beta1=beta0; beta2=beta1;
for t=1:T
    n=ceil(rand*3);
    y=randn(1,n); % make proposal
    if rand<min(f(y)/f(x)*g(x)/g(y),1)
        x=y; m=n;    % accept or reject proposal
    end
    data(t)=m;
    % determine parameters for each 'm'
    if m==1
        beta0=beta0+x;
    elseif m==2
        beta1=beta1+x;
    elseif m==3
        beta2=beta2+x;
    end
end
beta0=beta0/sum(data==1);
beta1=beta1/sum(data==2);
beta2=beta2/sum(data==3);
% plot models for each 'm'
plot(a,z,'.'), hold on
y2 = beta1(1)*A(:,1) + beta1(2)*A(:,2);
y3 = beta2(1)*A(:,1) + beta2(2)*A(:,2)  + beta2(3)*A(:,3);
plot(a,y2,'r');
plot(a,y3,'b');
figure(2), prob=[sum(data==1),sum(data==2),sum(data==3)]/T;
stem(prob), xlim([0.8,3.5]) % plot posterior model probabilities

