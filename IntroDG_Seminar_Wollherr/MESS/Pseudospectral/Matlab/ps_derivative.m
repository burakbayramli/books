
clear
close all


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
df=-2/s^2*x.*exp(-1/s^2*x.^2);

% Calculate numerical derivative using Fourier method

% initialize k vector up to Nyquist wavenumber
kmax=pi/dx;
dk=kmax/(nx/2);
for i=1:nx/2, k(i)=(i)*dk; k(nx/2+i)=-kmax+(i)*dk; end
k=sqrt(-1)*k;

% FFT and IFFT
ff=fft(f); % Transform  to wavenumber domain
ff=k.*ff;  % multiply by ik
dfn=real(ifft(ff)); % Transform back to space domain
    
% Plot analytical and numerical result
plot(x,f,'-',x,df,'--',x,dfn,'r-')
axis([-1 1 -Inf Inf ])
xlabel(' x ')
ylabel(' f(x) and d/dx f(x) ')
legend('f(x)','d/dx f(x) - analytical ','d/dx f(x) - numerical ')

% Calculate error
err=sum((dfn-df).^2)/sum(df.^2)*100;
title(sprintf(' Error %g %% ',err))