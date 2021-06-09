function [w,fave] = weeklyAve(t,f)
% weeklyAve  Compute average weekly flow rate from hourly flow data
%            Used for analysis of Glen Canyon Dam data
%
% Synopsis:  [w,fave] = weeklyTotal(t,f)
%
% Input:     t = time in hours from the begining of the year.
%            f = hourly flow rate (cfs)
%
% Output:    w    = vector of integer weeks from beginning of the year
%            fave = vector of average flow rates for each week of the year

n = length(f);
hoursPerWeek = 24*7;
nweeks = floor(n/hoursPerWeek);
if n/hoursPerWeek-nweeks > 0.01
  fprintf('Warning in weeklyAve:\n');
  fprintf('\tInput does not divide into integer number of weeks per year\n');
  nweeks = ceil(n/hoursPerWeek);
end
w    = (1:nweeks)';
flowAve = zeros(size(w));
k = 0;
nptsPerWeek = hoursPerWeek-1;
for i=1:nptsPerWeek:n
  k = k + 1;
  iend = min(n,i+nptsPerWeek);
  fave(k) = trapzDat(t(i:iend),f(i:iend));
end
fave = fave/hoursPerWeek;   %  Divide each sum by the total interval
