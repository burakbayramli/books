


%                           Matlab program heat4
%
%      This program integrates the heat equation u_t - ku_xx = q(x) on
%   the interval [0, 10] with Neumann boundary conditions u_x(0,t) =
%   u_x(10,t) = 0.  Crank-Nicholson method is used with delx = .05
%   and delt = .05.
%      At run time user must enter the value of the diffusion
%   coefficient k. User also must enter the number of time steps
%   n1, n2, n3, and n4 between snapshots as in program heat3.
%   Output is written into the vectors 'snap0',snap1', 'snap2','snap3'
%   and 'snap4'.  The vectors may be plotted alone or together.
%      The initial data comes from the file heatf.m.  User must 
%   provide a function mfile q.m for the source.
     



disp('  ')
k = input('enter the value of the diffusion coefficient k  ')
disp(' Enter the number of time steps between snapshots, n1,n2, n3, n4 ')
n = input(' in the form [n1 n2 n3 n4]    ')
n1 = n(1);  n2 = n(2); n3 = n(3);  n4 = n(4); 
delt = .05
t1 = n1*delt
t2 = (n1 +n2)*delt
t3 = (n1 + n2 +n3)*delt
t4 = (n1 + n2 + n3 +n4)*delt

delx = .05
r = .5*k*delt/(delx^2);

J = 200 

x = 0:delx:10;

    snap0= heatf(x);
    qq = q(x);

v= snap0;

D= sparse(1:J+1, 1:J+1, (1+2*r), J+1,J+1, J+1);

D(1,1) = D(1,1) - r;

D(J+1, J+1) = D(J+1, J+1) - r;

E = sparse(2:J+1, 1:J, -r, J+1,J+1,J+1);

A = D + E+ E';

[L,U] = lu(A);


for n = 1:n1
     b = 2*v + delt*qq;
     y = L\b';
     z = (U\y)';
     v = z - v;
end
     snap1 = v;
disp('Computed up to time t1 ')

for n = n1+1: n1+n2

     b = 2*v +delt*qq;
     y = L\b';
     z = (U\y)';
     v = z-v;
end
     snap2 = v;
disp('Computed up to time t2')

for n = n1 + n2+1:n1 + n2 +n3

     b = 2*v +delt*qq;
     y = L\b';
     z = (U\y)';
     v = z-v;
end
    snap3 = v;
disp('Computed up to time t3')

for n = n1 + n2 + n3+1:n1 + n2 + n3 +n4

     b = 2*v +delt*qq;
     y = L\b';
     z = (U\y)';
     v = z-v;
end

    snap4 = v;
disp('Computed up to time t4')

plot(x,snap0,x,snap1,x,snap2,x,snap3,x,snap4)

