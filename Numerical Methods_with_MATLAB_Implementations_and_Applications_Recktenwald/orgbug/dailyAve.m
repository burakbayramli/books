function [d,fave] = dailyAve(t,f)
% dailyAve   Compute average daily flow rate from hourly flow data
%            Used for analysis of Glen Canyon Dam data
%
% Synopsis:  [d,fave] = dailyTotal(t,f)
%
% Input:     t = time in hours from the begining of the year.
%            f = hourly flow rate (cfs)
%
% Output:    d    = vector of integer days from beginning of the year
%            fave = vector of average flow rates for each day of the year

n = length(f);          %  Number of points in input data
ndays = floor(n/24);    %  Number of integer days in the data set
if n/24-ndays > 0.01
  fprintf('Warning in dailyAve:\n');
  fprintf('\tInput does not divide into integer number of days per year\n');
  ndays = ceil(n/24); 
end
d = (1:ndays)';
fave = zeros(size(d));
k = 0;                               %  Counter for the days
for i=1:24:n
  k = k + 1;
  fave(k) = trapzDat(t(i:i+23),f(i:i+23));  % integrate flow*dt
end
fave = fave/24;
