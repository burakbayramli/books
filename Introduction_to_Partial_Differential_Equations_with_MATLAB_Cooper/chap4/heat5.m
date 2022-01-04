

%                         Matlab program heat5
%
%      This program integrates the heat equation u_t -ku_xx = 0 on
%    the interval 0 < x < 10, with prescribed boundary values 
%    u(0,t) = left(t), and u(10,t) = right(t). The diffusion
%    is piecewise constant: for x > 0, k = 1, while for x < 0
%    k = kleft which must be entered at run time. The Crank-Nicholson
%    is used with delx = .05 but now delt = .025.
%      As in programs heat3 and heat4 the programs computes four
%    snapshots of the solution at times t1, t2,t3,and t4 in
%    addition to the initial data which is snap0. 
%      At run time, user must enter the diffusion coefficient kleft,
%    and the number of time steps n1, n2, n3, and n4 between snapshots.
%    n1 is the number of time steps to snap1, n2 the number of time steps
%    from snap1 to snap2, etc. Vectors snap0, .. .. snap4 may be plotted
%    separately or together.
%       Also required: a function file heatf.m for the inital data
%    f(x), and function files left.m and right.m for the boundary values
%    u(0,t) = left(t), and u(10,t) = right(t). 




kleft = input('enter the value of the diffusion coeff. kleft  ')
disp(' Enter the number of time steps between snapshots, n1, n2, n3, n4 ')
n = input(' in the form [n1 n2 n3 n4]       ')

n1 = n(1);  n2 = n(2);  
n3= n(3);  n4 = n(4);
delt = .025
t1 = n1*delt
t2 = (n1 +n2) *delt
t3 = (n1 + n2 +n3)*delt
t4 = (n1 + n2 + n3+n4)*delt

delx = .05

sright = .5*delt/(delx^2);
sleft = .5*kleft*delt/(delx^2);

J =  10/delx;

x = 0:delx:10;

    snap0 = heatf(x);


for j = 1:J/2 -1
    diag(j) = 1+2*sleft;
    diag(j+J/2) = 1+2*sright;
end
diag(J/2) = sleft + sright;

D= sparse(1:J-1, 1:J-1, diag, J-1, J-1, J-1);

for j = 1:J/2-1
    lower(j) = -sleft;
    lower(j+J/2 -1) = -sright;
end

E = sparse(2:J-1, 1:J-2, lower,J-1, J-1,J-1);

A = D + E + E';

[L,U] = lu(A);

e= [-sleft, sleft + sright - 2, -sright ];

C = sparse( J/2, [J/2-1, J/2, J/2+1], e, J-1, J-1, J-1);


v = snap0(2:1:J);

% special setup for the first time step when the initial data and
% the boundary data are discontinuous in the corners.

    b = 2*v' + C*v';
    b(1) = b(1) + sleft*( left(delt)+ snap0(1)  );
    b(J-1) = b(J-1) +sright*( right(delt) + snap0(J+1) );
    y = L\b;
    z = (U\y)' ;
    v = z-v ;

for n = 2:n1
     b = 2*v'+C*v' ;
     b(1) = b(1) + sleft*( left(n*delt) + left((n-1)*delt) );
     b(J-1) = b(J-1) +sright*( right(n*delt) + right((n-1)*delt) );
     y = L\b;
     z = (U\y)';
     v = z - v;
end
     snap1 = [left(n*delt),v,right(n*delt)];
disp('Computed up to time t1 ')

for n = n1+1:  n1 + n2

     b = 2*v' + C*v';
     b(1) = b(1) +sleft*( left(n*delt) + left((n-1)*delt) );
     b(J-1) = b(J-1) +sright*( right(n*delt) +right((n-1)*delt) );
     y = L\b;
     z = (U\y)';
     v = z-v;
end
     snap2 = [left(n*delt),v,right(n*delt)];
disp('Computed up to time t2')

for n = n1 + n2+1: n1 + n2 +n3
     b = 2*v' +C*v';
     b(1) = b(1) + sleft*( left(n*delt) + left((n-1)*delt) );
     b(J-1) = b(J-1) +sright*( right(n*delt) + right((n-1)*delt) );
     y = L\b;
     z = (U\y)';
     v = z-v;
end
    snap3 = [left(n*delt),v,right(n*delt)];
disp('Computed up to time t3')

for n =n1 + n2 + n3+1:n1 + n2 + n3 +n4
     b = 2*v' + C*v';
     b(1) = b(1) +sleft*( left(n*delt) + left((n-1)*delt) );
     b(J-1) = b(J-1) + sright*(right(n*delt) + right((n-1)*delt) );
     y = L\b;
     z = (U\y)';
     v = z-v;
end

    snap4 = [left(n*delt),v,right(n*delt)];
disp('Computed up to time t4')

plot(x,snap0,x,snap1,x,snap2,x,snap3,x,snap4)

