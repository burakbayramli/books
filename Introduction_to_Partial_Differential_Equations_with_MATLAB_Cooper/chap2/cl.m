

%                    Matlab program cl (conservation law)
%
%      This program uses an upwind finite difference code to solve the
%    initial value problem  u_t + (F(u))_x= 0,  u(x,0) = f(x) .
%    The u_t is approximated by a forward difference and (F(u))_x
%    by a backward difference.  The method is dissipative and
%    tend to smear out the shocks.
%      User must provide a flux function F(u). It is required that
%    c(u) = F'(u) > 0 for all values of u in the problem. As an
%    example F(u) = .5*u^2 is given.
%      It is further assumed that the left boundary value is constant.
%      The program calls the function "stepcl" which integrates the
%    finite difference scheme "nsteps" time steps foward in time.
%      There are six choices of initial data. The first two are the same
%    as for the program mtc, given in the mfiles f1.m and f2.m.  They 
%    give rise to shock waves.  The third and fourth are simple step
%    functions given in the body of the program.  The fifth and sixth are
%    combinations of humps and are given in the function mfiles f5.m 
%    and f6.m. User may make his own choice of initial data in the 
%    function f7.m.  The function must be array smart.
%       At run time user must make the choice of data: 1,2,3,4,5,6, or 7.
%    User must also enter the size of the spatial step size delx, and the
%    size of the time step delt. delt and delx must be chosen to 
%    satisfy the CFL condition. 
%       Also at run time the user must enter the number n1 of time steps to
%    time t1, the number of time steps n2 from time t1 to  time t2, etc.
%    For example if delt = .05, then n1 = n2 = 20, and n3 = 10, n4 = 20 
%    yields the times t1 = 1, t2 = 2, t3 = 2.5, and t4 = 3.5.
%
%       The program saves for viewing snapshots of the solution at times
%    t = 0 (snap0), t = t1 (snap1), t = t2 (snap2), etc. All five snapshots
%    are plotted together on the interval -1 < x < 6. To plot one of
%    them separately, say snap2, use the command 
%
%                            plot(x,snap2)
%
%    To see two of them together, say snap2 and snap4, use the command
%
%                        plot(x,snap2,x,snap4) 
%
disp('  ')
disp('Enter 1 for decreasing profile -  develops shock ')
disp('Enter 2 for another of the same ' )
disp('Enter 3 for step shock wave   ')
disp('Enter 4 for centered rarefaction wave ')
disp('Enter 5 for single hump  ')
disp('Enter 6 for hump and valley ')
disp('Enter 7 for your choice    ')
m = input('Enter 1,2,3,4,5,6, or 7   ')



delx = input('Enter the spatial step delta x    ')
delt = input('Enter the time step delta t    ')

N = input(' Enter the number of time steps in the form [n1, n2, n3, n4]  ')
n1 = N(1);  
t1 = n1*delt
n2 = N(2);
t2 = (n1+n2)*delt
n3 = N(3);
t3 =  (n1 + n2 +n3)*delt
n4 = N(4);
t4 =  (n1 + n2 + n3 + n4)*delt


rho = delx/delt;

x = -1:delx:6;

jmax = length(x);


if m == 1
      u = f1(x);

elseif m == 2
      u = f2(x);

elseif m == 3
      u = (x< delx/2);

elseif m == 4
      u = (x > -delx/2);

elseif m == 5
      u = f5(x);

elseif m == 6
      u = f6(x);

elseif m == 7
      u = f7(x);
end

snap0 = u;

j = [2:jmax];

for n =1:n1
   u(j) = u(j) -( flux(u(j)) - flux(u(j-1)) )/rho ;
end
snap1 = u;

for n = 1:n2
   u(j) = u(j) -( flux(u(j)) - flux(u(j-1)) )/rho;
end
snap2 = u;

for n = 1:n3
   u(j) = u(j) -( flux(u(j)) - flux(u(j-1)) )/rho;
end
snap3 = u;

for n = 1:n4
   u(j) = u(j) -( flux(u(j)) - flux(u(j-1)) )/rho;
end
snap4 = u;

plot(x, snap0)
hold on
plot(x,snap1)
plot(x,snap2)
plot(x,snap3)
plot(x,snap4)
hold off









