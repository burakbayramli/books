
%                  Program mtc (method of characteristic)
%
%     This program solves the initial value problem 
%
%            u_t + uu_x = 0,  u(x,0) = f(x)
%
%   using the method of characteristics.  There are four choices of
%   initial data. Choices 1 and 2 should develop a shock, but instead
%   'roll over' into a curve which no longer defines a function.
%    Choice 3 is a rarefaction wave.  The initial data for choice 4
%   is a small perturbation of the constant function f(x) = 1. In this
%   case the program computes the solution of the nonlinear equation 
%   and the solution of the linearized problem which is u_t + u_x = 0.
%       At run time user enters the choice of data, 1,2,3, or 4, and
%   the times t1,t2,t3,and t4 at which the solution is to be viewed.
%   The solution profiles at times t = 0. t1,t2,t3,t4 are all plotted on
%   the interval [-1 6]. The solution profiles of both linear and nonlinear
%   problems are plotted together in the case of data option 4.


disp('  ')
disp('Enter 1 for decreasing profile  ')
disp('Enter 2 for another decreasing profile  ')
disp('Enter 3 for increasing profile  ')
disp('Enter 4 for the small perturbation of f(x)= 1 ')
m = input( 'enter the choice of initial data: 1,2,3,or 4   ')

t = input(' Enter the times in the form  [t1 t2 t3 t4 ]  ')


x = -1:.05:6;
n = length(x);

if m == 1

     y = f1(x);


elseif m == 2

     y = f2(x);

elseif m == 3

     y = f3(x);

elseif m == 4

     y = f4(x);

end


   x1 = x + t(1)*y;
   x2 = x + t(2)*y;
   x3 = x + t(3)*y;
   x4 = x + t(4)*y;
   plot(x,y)
   hold on
   plot(x1,y)
   plot(x2,y)
   plot(x3,y)
   plot(x4,y) 

if m == 4

  xl1 = x + t(1);
  xl2 = x + t(2);
  xl3 = x + t(3);
  xl4 = x + t(4);


  plot(xl1,y,'g')
  plot(xl2,y,'g')
  plot(xl3,y,'g')
  plot(xl4,y,'g')
else

end

set(gca, 'XLim',[-1 6])

hold off


