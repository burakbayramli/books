
%          Matlab program shocks
%
%     This program solves the initial value problem 
%
%            u_t + uu_x = 0,  u(x,0) = f(x)
%
%   using the method of characteristics, modified to capture shocks when 
%   they form.  There are four choices of initial data. Choices 1 and 2
%   develop a shock. Choice 3 is a step function which also yields a 
%   shock.
%       At run time user enters the choice of data, 1,2, or 4 and
%   the times t1,t2,t3,and t4 at which the solution is to be viewed.
%   Solutions are all plotted on the interval -1 < x < 6.


disp( '  ')
disp(' Enter 1 for decreasing profile - develops shock  ')
disp(' Enter 2 for another decreasing profile  ')
disp(' Enter 3 for step  ')
m = input( 'enter the choice of initial data: 1,2, or 3 ')

t = input(' Enter the times in the form [t1 t2 t3 t4]       ')

t1 = t(1);
t2 = t(2);
t3 = t(3);
t4 = t(4);

x = -1:.05:6;
n = length(x);

if m == 1

   y = f1(x);

elseif m == 2
  
   y = f2(x);

elseif m == 3

    y = (x < 0);

end

if m == 3
   x1 = x + .5*t1;
   x2 = x + .5*t2;
   x3 = x + .5*t3;
   x4 = x + .5*t4;
else

s = y(41);


x1 = zeros(size(x));
for j  = 1:141
   r = (x(j) + t1*y(j)-(1 +s*t1))*(x(j)-1);
   if r > 0
       x1(j) = x(j) + t1*y(j);
   else
       x1(j) = 1+s*t1;
   end
end

x2 = zeros(size(x));
for j = 1:141
    r = (x(j)+ t2*y(j) - (1+s*t2))*(x(j)-1);
    if r > 0 
       x2(j) = x(j) +t2*y(j);
    else
       x2(j) = 1 + s*t2;
    end
end

x3 = zeros(size(x));
for j = 1:141
   r = (x(j) + t3*y(j)-(1+s*t3))*(x(j)-1);
   if r > 0
       x3(j) = x(j) +t3*y(j);
   else
       x3(j) = 1 + s*t3;
   end
end

x4 = zeros(size(x));
for j = 1:141 
   r = (x(j) + t4*y(j)-(1+s*t4))*(x(j) -1);
   if r > 0
       x4(j) = x(j) + t4*y(j);
    else
       x4(j) = 1 +s*t4;
    end
end
end
   plot(x,y)
   hold on
   plot(x1,y,'g')
   plot(x2,y,'g')
   plot(x3,y,'g')
   plot(x4,y,'g') 



set(gca, 'XLim',[-1 6])

hold off


