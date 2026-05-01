function [a_fit, sig_a, yy, chisqr] = pollsf(x, y, sigma, M)
% Function to fit a polynomial to data
% Inputs 
%   x       Independent variable
%   y       Dependent variable
%   sigma   Estimate error in y
%   M       Number of parameters used to fit data
% Outputs
%   a_fit   Fit parameters; a(1) is intercept, a(2) is slope
%   sig_a   Estimated error in the parameters a()
%   yy      Curve fit to the data
%   chisqr  Chi squared statistic

%* Form the vector b and design matrix A
b = y./sigma;    
N = length(x);
for i=1:N
 for j=1:M
  A(i,j) = x(i)^(j-1)/sigma(i);  
 end
end

%* Compute the correlation matrix C 
C = inv(A.' * A);

%* Compute the least squares polynomial coefficients a_fit
a_fit = C * A.' * b.';  

%* Compute the estimated error bars for the coefficients
for j=1:M
  sig_a(j) = sqrt(C(j,j));   
end                         

%* Evaluate curve fit at each data point and compute Chi^2
yy = zeros(1,N);
for j=1:M
  yy = yy + a_fit(j)*x.^(j-1);  % yy is the curve fit
end
chisqr = sum( ((y-yy)./sigma).^2 );
return;
