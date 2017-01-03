% Script to demonstrate equality of two floating point numbers

x = tan(pi/6);
y = sin(pi/6)/cos(pi/6);
if x==y
  fprintf('x and y are equal\n');
else
  fprintf('x and y are not equal:  x = %e  y = %e\n',x,y);
  fprintf('x-y = %e\n',x-y);
end
