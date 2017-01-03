function y = noneg2(x)
% noneg2  Returns x if x>0, otherwise prints an error message and stops.
%         The error message contains the value of x.
if x<0
  error(sprintf('x = %f cannot be negative',x));
end
y = x;
