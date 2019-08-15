function demo2
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Test of method of Goldfarb/Idnani
% for linear-quadratic optimization problems
% f(x) = x'*A*x/2 - a'*x = Min!,
% g(x) = B*x + b >= 0; no equations

clc, format short, format compact
disp(' Example 3 with figure ')
nr = 100; KK = [1,2,3,4,5];
while ~ismember(nr,KK)
   nr   = input(' Example no. (1/2/3/4/5) ');
end;

switch nr
case 1, disp(' Example of Goldfarb 2 ')
   A = [1, 0 ,0; 0, 1, 0; 0, 0, 1];
   B = [-4, -3, 0; 2, 1, 0; 0, -2, 1];
   a = [0;5;0]; b = [8; -2; 0];
   x_opt = [10; 22; 44]/21;
case 2, disp(' Example with random data ')
   n = 5; m = 4;
   IB = [1:m]; % then also dlqp_h.m
   A     = 2*rand(n,n) - 1; B = 2*rand(m,n) - 1;
   x_opt = 10*rand(n,1) - 5;
   s     = rand(m,1); v = 30*rand(m,1);
   for i = 1:n
      A(i,i) = 0;
      A(i,i) = sum(abs(A(i,:))) + rand + 1;
   end
   for i = 1:m
      D = B(i,:)/norm(B(i,:));
      B(i,:) = D;
   end
   s(IB) = 0;
   u = zeros(m,1); u(IB) = v(IB);
   b = s - B*x_opt; a = A*x_opt - B'*u;
case 3, disp(' Example Spellucci p. 301, with figure ')
   A =   [1, 0; 0, 1];
   a =   [-10; -2];
   B = [1, 1; 0, -1; -1, -1; 50, -50;
       -1, 0; 0, 10; 2, 4; 4, -8];
   b = [0; 3; 5; 100; 5; 10; 0; 16];
   x_opt = [-1;1];
   % x = [0; 0] % feasible point
case 4, disp(' Example of Goldfarb and Idnani p. 29 ')
   A = [4, -2; -2, 4]; a = [-6; 0];
   B = [1, 0; 0, 2; 1, 1];
   b = [0; 0; -2];
   x_opt = [1/2; 3/2];
   % x = [1 ; 1] % feasible point
case 5, disp(' Example of Goldfarb and Idnani II, p. 231 ')
   A = [1, 0, 0; 0, 1, 0; 0, 0, 1];
   a = [0; 5; 0];
   B = [-4, -3, 0; 2, 1, 0; 0, -2, 1];
   b = [8; -2; 0];
   x_opt = [10;22;44]/21;
   % x = [1; 0; 0] % feasible point
end
if nr ~= 3
   [x,y,f,errorcode] = dlqp_g(A,a,B,b);
   diff_x = norm(x-x_opt)
else
   [x,y,f,errorcode,Pfad] = dlqp_g(A,a,B,b);
   save daten Pfad
   Pfad
   fig0310
end
