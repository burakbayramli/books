


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chebyshev derivative
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all
close all


% space domain
nx=200;     % number of grid points in x 


for ix=0:nx, x(ix+1)=cos(pi*ix/nx);  end
dxmin=min(abs(diff(x)));  % calculate minmum space increment
dxmax=max(abs(diff(x)));  % calculate maximum space increment
x=x'; % Transpose to have column vector

% Function Example: Gaussian
% width of Gaussian
s=.2;
% Gaussian function (modify!)
f=exp(-1/s^2*x.^2);
% Analytical derivative
df=-2/s^2*x.*exp(-1/s^2*x.^2);


D=get_cheby_matrix(nx);  % Initialize differentiation matrix

dfn=D*f;

% Plot analytical and numerical result
plot(x,f,'-',x,df,'--',x,dfn,'r-')
axis([-1 1 -Inf Inf ])
xlabel(' x ')
ylabel(' f(x) and d/dx f(x) ')
legend('f(x)','d/dx f(x) - analytical ','d/dx f(x) - numerical ')

% Calculate error
err=sum((dfn-df).^2)/sum(df.^2)*100;
title(sprintf(' Error %g %% ',err))