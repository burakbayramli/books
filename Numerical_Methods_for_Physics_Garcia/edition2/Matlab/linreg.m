function [a_fit, sig_a, yy, chisqr] = linreg(x,y,sigma)
% Function to perform linear regression (fit a line)
% Inputs
%   x       Independent variable
%   y       Dependent variable
%   sigma   Estimated error in y
% Outputs
%   a_fit   Fit parameters; a(1) is intercept, a(2) is slope
%   sig_a   Estimated error in the parameters a()
%   yy      Curve fit to the data
%   chisqr  Chi squared statistic

%* Evaluate various sigma sums
sigmaTerm = sigma .^ (-2);
s = sum(sigmaTerm);              
sx = sum(x .* sigmaTerm);
sy = sum(y .* sigmaTerm);
sxy = sum(x .* y .* sigmaTerm);
sxx = sum((x .^ 2) .* sigmaTerm);
denom = s*sxx - sx^2;

%* Compute intercept a_fit(1) and slope a_fit(2)
a_fit(1) = (sxx*sy - sx*sxy)/denom;
a_fit(2) = (s*sxy - sx*sy)/denom;

%* Compute error bars for intercept and slope
sig_a(1) = sqrt(sxx/denom);
sig_a(2) = sqrt(s/denom);

%* Evaluate curve fit at each data point and compute Chi^2
yy = a_fit(1)+a_fit(2)*x;     % Curve fit to the data
chisqr = sum( ((y-yy)./sigma).^2 );  % Chi square
return;
