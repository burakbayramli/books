function demoXexp(n)
% demoXexp   Fit synthetic data to y = c(1)*x*exp(c(2)*x)
%
% Synopsis:  demoXexp(n)
%
% Input:     n = (optional) number of points to generate in the synthetic
%                data set;  Default: n = 200
%
% Output:    Fit coefficients are printed and a plot is created

if nargin<1,  n=200;  end

% --- Generate synthetic data and fit it 
x0 = 0.01;                            %  Starting point ~= 0 avoids log(0)
noise = 0.05;                         %  Magnitude of noise
x = linspace(x0,2,n);
y = 5*x.*exp(-3*x);                   %  Create the true function y = g(x)
yn = y + noise*(rand(size(x))-0.5);   %  Noise multiplies random values v
                                      %  in the range -0.5 <= v <= 0.5
yn = abs(yn);                         %  Make sure all data are positive
c = xexpfit(x,yn);
fprintf('Fit parameters are c1 = %5.3f  c2 = %5.3f\n',c);

% --- Plot data and the fit function 
xfit = linspace(min(x),max(x));
yfit = c(1)*xfit.*exp(c(2)*xfit);
if n<30,  s = 'v';  else s = '-';  end   %  Select symbol for original data
plot(x,y,s,x,yn,'o',xfit,yfit,'--');
xlabel('x');   ylabel('y');  legend('original','noisy','fit');
xmax = max(x);   ymax = max(y);
text(0.5*xmax,0.7*ymax,sprintf('c1 = %5.3f   c2 = %5.3f',c));
text(0.5*xmax,0.6*ymax,sprintf('%d points in synthetic data set',n));
