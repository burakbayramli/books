%ou_timechange_ex.m
T=4; % final time
N=10^4; % number of steps
theta=2;mu=1;sig=0.2; % parameters
x=nan(N,1); x(1)=0;     % initial point 
h=T/(N-1); % step size
t=0:h:T;   % time
% code the right-hand side of the updating formula
f=@(z,x)(exp(-theta*h)*x+sig*sqrt((1-exp(-2*h*theta))/(2*theta))*z);
for i=2:N
     x(i)=f(randn,x(i-1));   
end
x=x+mu*(1-exp(-theta*t'));
plot(t,x)
