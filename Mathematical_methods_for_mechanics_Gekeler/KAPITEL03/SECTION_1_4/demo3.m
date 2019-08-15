function demo3
% Eckart Gekeler, Universitaet Stuttgart, Release 27/01/09
% Test of method of Goldfarb/Idnani 
% for linear-quadratic optimization problems
% with random data
% f(x) = x'*A*x/2 - a'*x = Min!,
% g(x) = B*x + b >= 0; C*x + c = 0;
clc, format short, format compact
example = 2;
switch example 
case 1
   n = 30; m = 20;
   IB = [1:m]; % dann auch dlqp_h.m
   IBH = [1:5];
   IB = [IBH, 7,9,10,13,15];
   A  = 2*rand(n,n) - 1; B = 2*rand(m,n) - 1;
   x_opt = 10*rand(n,1) - 5;
   s = rand(m,1); v = 30*rand(m,1);
   for i = 1:n
      A(i,i) = 0; A(i,i) = sum(abs(A(i,:))) + rand + 1;
   end
   for i = 1:m
      D = B(i,:)/norm(B(i,:)); B(i,:) = D;
   end
   s(IB) = 0;
   u = zeros(m,1); u(IB) = v(IB);
   b = s - B*x_opt; a = A*x_opt - B'*u;
   C = B(1:5,:); B = B(6:m,:); c = b(1:5); b = b(6:m);
case 2
   n = 3; m = 9; 
   [A,a,B,b,x_opt,u,C,c] = randdata(m,n);
end
[x,y,z,f,errorcode] = dlqp(A,a,B,b,C,c);
if example == 1
diff_x_y_z = [norm(x-x_opt),norm(y-u(6:m)),norm(z-u(1:5))]
end
if example == 2 
   diff_x = norm(x-x_opt)
end   
