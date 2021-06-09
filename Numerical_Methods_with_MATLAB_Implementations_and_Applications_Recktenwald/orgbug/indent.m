% indent  Script demonstrating advantages of indentation
%
%  NOTE:  nx needs to be set in the workspace

x = rand(1,nx) - 0.5

n = length(x);
if mean(x)<0
  y(1) = 0;  y(n) = 0;
  for k=2:n-1
     y(k) = x(k+1)-x(k-1);
  end
else
  y(1) = x(1);  y(n) = x(n);
  for k=2:n-1
     y(k) = 0.5*(x(k+1)+x(k-1));
  end
end
y  %  print y for comparison

n = length(x);
if mean(x)<0
y(1) = 0;  y(n) = 0;
for k=2:n-1
y(k) = x(k+1)-x(k-1);
end
else
y(1) = x(1);  y(n) = x(n);
for k=2:n-1
y(k) = 0.5*(x(k+1)+x(k-1));
end
end
disp(y);  %  print y for comparison
