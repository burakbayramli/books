
clear
close all

% Exercise with  Taylor operators

% Purpose:
% Quantify improvement of accuracy with high-order finite difference
% operators


% Initialize arbitrary test function on regular grid

% regular x grid between -1 1 
x=linspace(-1,1);
nx=length(x);
dx=x(2)-x(1);

% Function Example: Gaussian
% width of Gaussian
s=.2;
% Gaussian function (modify!)
f=exp(-1/s^2*x.^2);
% Analytical derivative
df=-2/s^2*(x+dx/2).*exp(-1/s^2*(x+dx/2).^2);


% Get Taylor operator for centered (staggered) derivative
n=input(' Give length of operator (2,4,...) : ');
oper=-taylor(n)/dx;

% plot if you want
figure 
plot(oper)
title(' Taylor Operator ')
xlabel(' Weight ')
ylabel(' Index ')


% Calculate numerical derivative

% Loop over spatial grid (avoiding boundaries)
% Forward derivative (e.g., (f_i+1-f_i) stored in dfn_i defined at x_i+dx/2

dfn=f*0;  % Initialize to 0
for i=n/2+1:nx-n/2-1,
    asum=0;
    for j=1:n,
        asum=asum+oper(j)*f(i-n/2+j);
    end
    dfn(i)=asum;
end
    
% Plot analytical and numerical result
plot(x,f,'-',x,df,'--',x,dfn,'r-')
axis([-1 1 -Inf Inf ])
xlabel(' x ')
ylabel(' f(x) and d/dx f(x) ')
legend('f(x)','d/dx f(x) - analytical ','d/dx f(x) - numerical ')

% Calculate error
err=sum((dfn-df).^2)/sum(df.^2)*100;
title(sprintf(' Error %g %% ',err))