function demo1
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% masterfile for linear programming
% Problem(P): max(a*x, B*x <= c, b^ix = c^i, i = 1:p)
% Problem(D): min(y*c, y*B = a, y_i >= 0, i = p+1:m)
% INPUT:
%        matrix B, a,y row vectors, c,x column vectors
%        x feasible/vertex for (P), cf. ``EXAMPLE1.M''
%        y feasible/vertex for (D), cf. ``EXAMPLE1.M''
% OUTPUT:
%        x solution of primal problem
%        y solution of dual problem
%        f value of objective function
%        errorcode = 0: solution found
%        errorcode = 1: initial point x or y not feasible
%        errorcode = 2: solution does not exist
%        errorcode = 3: max. step number reached
%        errorcode = 4: feasible domain empty
method = 100;
%while ~ismember(method,[1,2,3,4,5,6,7,8,9,10,11,12])
%   method = input('method nr. = ? (1/2/3/4/5/6/7/8/9/10/11/12) ');
%end
method = 2;
% -- projection methods ------------------------------------
switch method
case 1
   %nr = input('example nr. = ? (1) ');
   nr = 1;
   [a,B,c,p,l,u,x,y,IB] = example1(nr);
   [x,y,f,errorcode]    = proj01(a,B,c);
case 2
   %nr = input('example nr. = ? (1/2/3/4/5/7/10) ');
   nr = 5;
   [a,B,c,p,l,u,x,y,IB]  = example1(nr);
   [x,y,f,errorcode]     = proj02(a,B,c,x);
case 3
   nr = input('example nr. = ? (1/2/3/4/5/7/10) ');
   [a,B,c,p,l,u,x,y,IB]  = example1(nr);
   errorcode             = 0;
   [x,y,f,errorcode]     = proj03(a,B,c,x,p,errorcode);
case 4, disp(' comparison of two methods ')
   nr = input('example nr. = ? (1-11) ');
   [a,B,c,p,l,u,x,y,IB]  = example1(nr);
   [x,errorcode]         = proj04(a,B,c,p);
   [x,y,f,errorcode1]    = proj03(a,B,c,x,p,errorcode);
   [x1,y1,f1,errorcode2] = proj05(a,B,c,p);
   errorcode1, errorcode2,
   f, f1
   A = [x'; x1']
case 5
   nr = input('example nr. = ? (1/2/3/4/5/6) ');
   [a,B,c,p,l,u,x,y,IB]  = example1(nr);
   [x,y,f,errorcode]     = proj05(a,B,c,p);
case 6
   nr = input('example nr. = ? (1/2/3/4/5/7/10) ');
   [a,B,c,p,l,u,x,y,IB]  = example1(nr);
   errorcode             = 0;
   [x,y,f,errorcode]     = proj06(a,B,c,p,l,u,x,errorcode);
case 7
   nr = input('example nr. = ? (1-11) ');
   [a,B,c,p,l,u,x,y,IB]  = example1(nr);
   [x,errorcode07]       = proj07(a,B,c,l,u,p);
   [x,y,f,errorcode06]   = proj06(a,B,c,p,l,u,x,errorcode07);
   F = [errorcode06  f]
   [B,c]                 = trans(B,c,l,u);
   [x1,y1,f1,errorcode05] = proj05(a,B,c,p);
   F1 = [errorcode05 f1]
   if abs(f1 - f) > 10e-6
      F = [errorcode06  f]
      F1 = [errorcode05 f1]
   end
   % -- simplex methods ---------------------------------------
case 8
   nr = input('example nr. = ? (6/8) ');
   [a,B,c,p,l,u,x,y,IB]  = example1(nr);
   errorcode = 0;
   [x,y,f,IB,errorcode]= simplx01(a,B,c,y,IB,errorcode);
case 9
   nr = input('example nr. = ? (1-11) ');
   [a,B,c,p,l,u,x,y,IB]  = example1(nr);
   [y1,IB,errorcode]     = simplx02(a,B,c);
   [x,y,f,IB,errorcode]  = simplx01(a,B,c,y1,IB,errorcode);
case 10, disp(' comparison of two methods ')
   nr = input('example nr. = ? (1-11) ');
   [a,B,c,p,l,u,x,y,IB] = example1(nr);
   p = 0;
   [y,IB,errorcode] = simplx04(a,B,c,p);
   [x,y,f,p,IB,errorcode]  = simplx03(a,B,c,p,y,IB,errorcode);
   [x1,y1,f1,IB,errorcode] = simplx05(a,B,c);
   f, f1,
case 11
   nr = input('example nr. = ? (1-11) ');
   [a,B,c,p,l,u,x,y,IB]  = example1(nr);
   [x,y,f,IB,errorcode]  = simplx05(a,B,c);
case 12
   nr = input('example nr. = ? (1-11) ');
   [a,B,c,p,l,u,x,y,IB]  = example1(nr);
   [x,y,f,errorcode]     = proj05(a,B,c,p);
   [xs,ys,fs,errorcode]  = simplx06(a,B,c,p);
   f, fs,
end
% ----------------------------------------------------------
switch errorcode
case 1, disp('x or y not feasible!')
case 2, disp('solution does not exist!');
case 3, disp('max. step number reached!');
case 4, disp('feasible domain empty!');
case 0, x = x', y, f
end
