function lintest
% lintest  Compare schemes for generating a vector of equally spaced values
%
fprintf('  n     norm(y1-y)    norm(y2-y)    norm(y3-y)\n');
for n=[4 5 6 9 10 20 50 100]
  y  = linspace(0,1,n);
  y1 = linsp1(0,1,n);     y2 = linsp2(0,1,n);     y3 = linsp3(0,1,n);
  e1 = norm(y1-y,'inf');  e2 = norm(y2-y,'inf');  e3 = norm(y3-y,'inf');
  fprintf('%4d  %12g  %12g  %12g\n',n,e1,e2,e3);
end
