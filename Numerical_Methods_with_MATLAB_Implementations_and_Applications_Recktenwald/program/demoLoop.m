% demoLoop  Script file to demonstrate for and while loops

fprintf('\nSum elements of a vector\n');
x = 1:5;                   %  create a row vector
sumx = 0;                  %  initialize the sum
for k = 1:length(x)
   sumx = sumx + x(k);
end
fprintf('sumx = %f\n',sumx)

fprintf('\nDisplay values of a sin(x)\n');
for x = 0:pi/15:pi
   disp(sin(x));
end

fprintf('\nDisplay average values of columns of a matrix\n');
A = [1 2 3; 4 5 6; 7 8 9; 10 11 12];    %  Create a matrix
for v = A                               %  Select a column of A
   disp(mean(v));                       %  Print average of that column
end


fprintf('\nDemonstrate a while loop\n');
x = 1;             %   initial value
while x > 0.01
   x = x/2;
end
fprintf('x = %f\n',x)

fprintf('\nIterative calculation of square root\n');
r = 5;  rold = 2*r;   delta=5e-6;
while abs(rold-r) > delta
   rold = r;                   %  save old value for convergence test
   r = 0.5*(rold + x/rold);    %  update the guess at the root
end
fprintf('r = %f\n\n',r)

fprintf('\nIterative calculation of square root\n');
r = 5;  rold = 2*r;   delta=5e-6;
maxit = 25;
it = 0;
while abs(rold-r) > delta & it<maxit
   rold = r;                   %  save old value for convergence test
   r = 0.5*(rold + x/rold);    %  update the guess at the root
   it = it + 1;                %  increment the iteration counter
end
fprintf('r = %f\n\n',r)
