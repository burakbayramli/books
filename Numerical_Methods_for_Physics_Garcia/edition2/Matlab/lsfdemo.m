% lsfdemo - Program for demonstrating least squares fit routines
clear all; help lsfdemo; % Clear memory and print header

%* Initialize data to be fit. Data is quadratic plus random number.
fprintf('Curve fit data is created using the quadratic\n');
fprintf('  y(x) = c(1) + c(2)*x + c(3)*x^2 \n');
c = input('Enter the coefficients as [c(1) c(2) c(3)]: ');
N = 50;                 % Number of data points
x = 1:N;                % x = [1, 2, ..., N]
randn('state',0);       % Initialize random number generator
alpha = input('Enter estimated error bar: ');
r = alpha*randn(1,N);   % Gaussian distributed random vector
y = c(1) + c(2)*x + c(3)*x.^2 + r;
sigma = alpha*ones(1,N);    % Constant error bar

%* Fit the data to a straight line or a more general polynomial
M = input('Enter number of fit parameters (=2 for line): ');
if( M == 2 )  
  %* Linear regression (Straight line) fit
  [a_fit sig_a yy chisqr] = linreg(x,y,sigma);
else          
  %* Polynomial fit
  [a_fit sig_a yy chisqr] = pollsf(x,y,sigma,M);
end

%* Print out the fit parameters, including their error bars.
fprintf('Fit parameters:\n');
for i=1:M
  fprintf(' a(%g) = %g +/- %g \n',i,a_fit(i),sig_a(i));
end

%* Graph the data, with error bars, and fitting function.
figure(1); clf;           % Bring figure 1 window forward
errorbar(x,y,sigma,'o');  % Graph data with error bars
hold on;                  % Freeze the plot to add the fit
plot(x,yy,'-');           % Plot the fit on same graph as data
xlabel('x_i'); ylabel('y_i and Y(x)');
title(['\chi^2 = ',num2str(chisqr),'    N-M = ',num2str(N-M)]);
