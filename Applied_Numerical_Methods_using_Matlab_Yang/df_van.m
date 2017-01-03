function dx=df_van(t,x) %,mu)
%Van der Pol differential equation
global mu
dx=zeros(size(x));
dx(1)=x(2);
dx(2)=mu*(1-x(1).^2).*x(2)-x(1);