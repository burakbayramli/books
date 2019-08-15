function demo4
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Test of method of Goldfarb/Idnani 
% for linear-quadratic optimization problems
% with random data
% f(x) = x'*A*x/2 - a'*x = Min!,
% g(x) = B*x + b >= 0; C*x + c = 0;
clc
format short, format compact
nr = 100; 
%while ~ismember(nr,[1,2])
%   nr   = input(' Example no. (1/2) ');
%end;
nr = 1;
switch nr
case 1
   n = 3; m = 9;
   n = 9; m = 27;
   [A,a,B,b,x_opt,u] = randdata(m,n);
   %load sing_example A a B b x_opt u % n= 2, m = 3 necessary
case 2, disp(' Example Spellucci p. 301, with figure ')
   A =   [1, 0; 0, 1];
   a =   [-10; -2];
   B = [1, 1; 0, -1; -1, -1; 50, -50;
       -1, 0; 0, 10; 2, 4; 4, -8];
   b = [0; 3; 5; 100; 5; 10; 0; 16];  
   x_opt = [-1;1];
   % x = [0; 0] % feasible point
end

[x,y,f,errorcode] = dlqp_g(A,a,B,b);
%diff_x_y = [norm(x-x_opt),norm(y-u)]
f1 = a.'*x; f2 = a.'*x_opt;
fx_fx_opt = [f1,f2]
%y_u = [y,u]  
Lagrangenorm_mit_y  = norm(A*x - a - B.'*y)
%Lagrangenorm_mit_u  = norm(A*x - a - B.'*u)
%y
flag = 0;
if flag == 1  
   disp(' duales problem nur fuer m <= n loesbar')
   INVA = inv(A);
   AA = B*INVA*B.'; aa = -(b + B*INVA*a); bb = zeros(m,1); BB = eye(m);
   [xx,yy,ff,errorcode] = dlqp_g3(AA,aa,BB,bb);
   xx
   ff = ff + 0.5*aa.'*INVA*aa; ff = - ff
   Lagrangenorm_mit_y  = norm(AA*xx - aa - BB.'*yy)
    % es muss y = xx gelten, da Probleme konvex !!
end
save data1 A a B b x_opt% u
 