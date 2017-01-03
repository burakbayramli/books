function y = nonegp(x)
% nonegp  Returns x if x>0, otherwise prints a message and pauses
if x<0
  disp('x is negative, press return to proceed');
  pause;
end
y = x;
