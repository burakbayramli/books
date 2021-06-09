function demoLogic
% demoLogic  Demonstrate logical comparison operators, print results to screen

a = 2,  b = 4
aIsSmaller = a < b

bIsSmaller = b < a

a = 2;  b = 4;
aIsSmaller = a < b;
bIsSmaller = b < a;
bothTrue = aIsSmaller & bIsSmaller

eitherTrue = aIsSmaller | bIsSmaller



x=-2,  y=5
if x >= y
   c = x^2 - y;
elseif x*y > 0.0     %  make sure y/x is positive
   c = log(y/x);
end
if ~exist('c','var')
  warning('c is not defined');
else
  disp(c)
end

if x >= y
   c = x^2 - y;
elseif x*y > 0.0     %  make sure y/x is positive
   c = log(y/x);
else
   warning('either x and y are both negative or x<y');
   fprintf('x = %f    y= %f\n',x,y);
end
if ~exist('c','var')
  warning('c is not defined');
else
  disp(c)
end
