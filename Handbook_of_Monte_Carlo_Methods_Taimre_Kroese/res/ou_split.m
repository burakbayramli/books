function [indicator,x,y,tau]=ou_split(gam,x_ini,y_ini)
% implements the exact sampling of an OU process
h=0.001; % step size
% OU updating formula for exact simulation
f=@(x,z)(exp(-h)*x+sqrt((1-exp(-2*h))/2)*z);
x(1)=x_ini; y(1)=y_ini;
tau_axis=inf; tau_circ=inf;
for i=2:10^7 % choose an arbitrarily large loop to ensure hitting
    x(i)=f(x(i-1),randn);
    y(i)=f(y(i-1),randn);
    if (x(i-1)*x(i)<0)|(y(i-1)*y(i)<0)
        tau_axis=h*i; % determine axis hitting time
    end
    if (x(i-1)^2+y(i-1)^2<gam^2)&(x(i)^2+y(i)^2>gam^2)
        tau_circ=h*i; % determine circle hitting time
    end
    tau=min(tau_circ,tau_axis);
    if isinf(tau)==0
        break
    end
end
indicator=tau_circ<tau_axis; %did we hit the circle first?
