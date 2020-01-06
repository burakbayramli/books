function y=log_normcdf(t)
%LOG_NORMCDF   Approximation of the log normal cdf.
%   LOG_NORMCDF(X) is a CVX compatible approximation of the
%   function log(normcdf(X)), the log of the standard normal cumulative
%   distribution function. It is reasonably accurate for |X| <= 3.
%   For vector arguments it works elementwise. For constant arguments
%   it returns the numeric value of the approximation, which can
%   be compared with the exact value, computed as log(normcdf(x)).
%   
%   Disciplined convex programming information:
%       LOG_NORMCDF is concave and nondecreasing.

n = size(t,1);

a =[ 0.018102332171520
   0.011338501342044
   0.072727608432177
   0.184816581789135
   0.189354610912339
   0.023660365352785];
breakpoints = [3 2.5 2 1 -1 -2]';
y=(-a'*square_pos(repmat(breakpoints,1,n)-repmat(t',6,1)))';
