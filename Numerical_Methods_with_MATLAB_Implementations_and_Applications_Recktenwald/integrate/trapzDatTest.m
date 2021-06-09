function trapzDatTest
% trapzDatTest  Verify trapzDat function for different types of input
%
% Synopsis:  trapzDatTest
%
% Input:     none
%
% Output:    Print out of test results

% --- Integral of a constant
fprintf('Integrate a constant function\n');
x = linspace(-5,7,8);      %  Test 1:  even number of intervals
f = pi*ones(size(x));      %  constant function
s = trapzDat(x,f);
err = s - pi*(max(x)-min(x));
fprintf('\tEven interval test:    s = %8.4f  error = %10.3e\n',s,err);

x = linspace(-5,7,9);      %  Test 2:  odd number of intervals
f = pi*ones(size(x));      %  constant function
s = trapzDat(x,f);
err = s - pi*(max(x)-min(x));
fprintf('\tOdd interval test:     s = %8.4f  error = %10.3e\n',s,err);

x = sort(rand(5,1)-0.2);   %  Test 3:  randomly spaced x data
f = pi*ones(size(x));
s = trapzDat(x,f);
err = s - pi*(max(x)-min(x));
fprintf('\tRandom spacing test:   s = %8.4f  error = %10.3e\n',s,err);

% --- Truncation error test
fprintf('\nTruncation error test\n');
fprintf('   n      integral          error        alpha\n');
xmin = -5*pi/7;  xmax =  3*pi/5;  % oddball starting and stoping points
hold = -5;       errold = 0;      % initialize test for first try

for n = [10  17  25  100  315  2001  4522]
  x = linspace(xmin,xmax,n);
  f = sin(x);
  s = trapzDat(x,f);
  err = s - (cos(x(1)) - cos(x(n)));
  fprintf('%5d  %15.12f  %12.3e',n,s,err);
  h = (xmax-xmin)/(n-1);
  if hold<0
    fprintf('\n');           %  first iteration, skip test
  else
    fprintf('  %9.5f\n',log(err/errold)/log(h/hold));
  end
  hold = h;  errold = err;    %  prepare for next iteration
end
