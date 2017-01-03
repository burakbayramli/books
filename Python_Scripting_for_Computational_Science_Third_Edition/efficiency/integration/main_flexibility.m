a = 0; b = 2; n = 10;

% function f1 defined in f1.m (function handle):
result = Trapezoidal(a, b, @f1, n);
disp(result);

% inline object f:
f = inline('exp(-x*x)*log(1+x*sin(x))');
result = Trapezoidal(a, b, f, n);
disp(result);

% string expression:
result = Trapezoidal(a, b, 'exp(-x*x)*log(1+x*sin(x))', n);
disp(result);



