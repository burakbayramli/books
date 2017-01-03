function y = noneg(x)
% noneg  Returns x if x>0, otherwise prints an error message and stops
if x<0
  error('x is negative');
end
y = x;
