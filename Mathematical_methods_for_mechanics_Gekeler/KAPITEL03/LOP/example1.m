function [a,B,c,p,l,u,x,y,IB] = example1(nr);
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Examples for DEMO1.M
switch nr
case 1
   p  = 0;
   %  m = 8, n = 3
   a  = [4  3  10];
   B  = [-1   0   0; 0  -1  0; 0   0  -1; 3   2   6;
          4   3   6;-1  -1  2; 0   1   0; 0   0   1];
   c  = [ 0   0   0  1200  1500   0  150   150]';
   x  = [0  0  0]';   %vertex for (P)
   IB = [1 2 3];      %basis  for x
   y  = NaN*ones(1,8);
   l  = - inf*ones(3,1);
   u  =   inf*ones(3,1);
case 2
   p  = 0;
   %  m = 5, n = 2;
   a  = [6  8];
   B  = [-3  5; 1  3; 3  2; -1  0; 0 -1];
   c  = [ 25  29  45  0  0]';
   x  = [2  3]'  %feasible for (P), x = [0; 0] vertex of (P)
   IB = []; % IB = [4  5];  %basis for x = [0; 0]
   y  = NaN*ones(1,5);
   l  = - inf*ones(2,1);
   u  =   inf*ones(2,1);
case 3
   p  = 0;
   % m = 7, n = 3;
   %example of Baumgarten with multiple solution,
   %for test of initial extreme point
   a  = [2 3 0];
   B  = [-1 0 0; 0 -1 0; 0 2 0; 2 3 0; 1 0 0;0 0 -1; 0 0 1];
   c  = [0 0 6 13 5 0 2]';
   x  = [2 2 1]'; %feasible for (P)
   IB = [];
   y  = NaN*ones(1,7);
   l  = - inf*ones(3,1);
   u  =   inf*ones(3,1);
case 4
   p  = 0;
   % m = 4, n = 2;
   % Best-Ritter p. 39
   a  = [-1  2];
   B  = [-1  2; 1  0; -1  0; 0 -1];
   c  = [4   8  -2  0]';
   x  = [5   1]'; %feasible for (P)
   IB = [];
   y  = NaN*ones(1,4);
   l  = - inf*ones(2,1);
   u  =   inf*ones(2,1);
case 5
   % m= 8, n = 6
   % example for projection method with p > 0
   % test for necessarity of Bland's rule
   a  = [0 -2 -4  0 -4  0];
   B  = [1  -3   6  -1  -1  -1; 0   2   2   1  -3  -1;
         - eye(6)];
   c  = [0   0   0   0   0   0   0   0]';
   x  = [0   0   0   0   0   0]' %vertex of (P);
   IB = [];
   y  = NaN*ones(1,8);
   l  = - inf*ones(6,1);
   u  =   inf*ones(6,1);
   p  = 0;
case 6
   p  = 0;
   % m = 8, n = 5;
   a  = [1200   1500    0   150   150];
   B  = [3   2   6   1   0   0   0   0;
         4   3   6   0   1   0   0   0;
        -1  -1   2   0   0   1   0   0;
         0   1   0   0   0   0   1   0;
         0   0   1   0   0   0   0   1];
   B = B';
   c  = [-4  -3  -10   0   0   0   0  0]';
   x  = NaN*ones(5,1);
   y  = [0  0  0  1200  1500  0  150  150];  %vertex of (D)
   IB = [4  5  6  7  8]; %basis for y
   l  = - inf*ones(5,1);
   u =  inf*ones(5,1);
case 7
   % example for projection method with p > 0
   % and bounded variables
   p  = 0;
   m  = 7;
   n  = 5;
   a  = 10*rand(1,n);
   b  = 10*rand(1,n);
   a  = a - b;
   B  = 10*rand(m,n);
   x  = ones(n,1);  %feasible for (P)
   IB = [];
   y  = NaN*ones(1,m+n);
   c  = 10*rand(m,1);
   c  = B*x + 6*c;
   if p > 0
      I = 1:p;
      c(I) = B(I,:)*x;
   end
   l  = - 5*ones(n,1);
   u  =   3*ones(n,1);
   u(1:2) = [7;6];
%   l  = - inf*ones(n,1);
%       u  =   inf*ones(n,1);
case 8
   p  = 0;
   a  = [4  3  10];
   B  = [1   0   0; 0  1  0; 0   0  1; 3   2   6;
         4   3   6;-1  -1  2; 0   1   0; 0   0   1];
   c  = [0   0   0  1200  1500  0  150  150]';
   x  = NaN*ones(3,1);
   y  = [4  3  10  0   0   0   0  0]; %vertex of (D)
   IB = [1 2 3]; %basis vector for y
   l  = - inf*ones(3,1);
   u  =   inf*ones(3,1);
case 9
   p = 0;
   m  = 6;
   n  = 4;
   c  = 10*rand(m,1);
   b  = 10*rand(m,1);
   c  = c - b;
   B  = 10*rand(m,n);
   x  = NaN*ones(n,1);
   y  = [10*rand(1,n) zeros(1,m-n)];
   IB = 1:n; %basis vector for y
   a = y*B;
   l  = - ones(n,1);
   u  =   ones(n,1);
case 10
   p  = 3;
   m  = 7;
   n  = 5;
   a  = 10*rand(1,n);
   b  = 10*rand(1,n);
   a  = a - b;
   B  = 10*rand(m,n);
   B  = [B; -eye(n)];
   x  = ones(n,1);  %feasible for (P)
   IB = [];
   y  = NaN*ones(1,m+n);
   c  = 10*rand(m,1);
   e  = 10*rand(n,1);
   c  = [c; e];
   c  = B*x + c;
   l  = - inf*ones(n,1);
   u  =   inf*ones(n,1);
   if p > 0
      I = 1:p;
      c(I) = B(I,:)*x;
   end
case 11
   p  = 2;
   m  = 6;
   n  = 4;
   a  = 10*rand(1,n);
   b  = 10*rand(1,n);
   a  = b - a;
   B  = 10*rand(m,n);
   x  = 10*rand(n,1);
   u  = 10*rand(p,1);
   x(1:p) = x(1:p) - u;
   IB = [];
   y  = NaN*ones(1,m);
   c  = B*x;
   l  = -inf*ones(n,1);
   u  =  inf*ones(n,1);
end
