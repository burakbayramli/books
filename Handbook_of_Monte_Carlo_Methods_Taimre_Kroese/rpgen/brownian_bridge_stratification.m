%brownian_bridge_stratification.m
K=7;   %number of strata
n=10^3; %number of points for path construction
T=1; % terminal time at which we stratify
t=[0:T/(n+1):T];
for i=1:K
    U=(i-1+rand)/K;  % stratified uniforms
    x_s=sqrt(T)*norminv(U); % stratified terminal value
    X=brownian_bridge(t,0,x_s,randn(n,1));
    plot(t,X), hold all
end

% Extra code for plotting purposes
x=-3:0.01:3; x=sqrt(T)*x;
y=normpdf(x,0,sqrt(T));
yy=norminv(([2:10]-1)/K,0,sqrt(T));
xx=T+normpdf(yy,0,sqrt(T));
plot(T+y,x,'-'),hold on
for i=1:9
    xxx=T:0.005:xx(i);
    plot(xxx,yy(i)*ones(size(xxx)),'-')
end
plot(T+y*0,x,'-')
