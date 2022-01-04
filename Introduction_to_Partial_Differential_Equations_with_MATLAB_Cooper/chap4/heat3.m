

%                          Matlab program heat3
%
%   This program integrates the heat equation  u_t -ku_xx = 0
%   on the interval [0,10] using the Crank-Nicholson method.
%   The implicit set of equations are solved at each time step
%   using an LU factorization. The time step delt = .05, and 
%   delx = .05 as well.
%      The program makes a snapshot, 'snap0', of the initial data
%   and four later snapshots 'snap1','snap1','snap3,'snap4' at
%   times t1,t2,t3, and t4. You may plot the vectors snap0, snap1, etc.,
%   separately or together.
%      At run time the user must enter the number n1 of time steps
%   to the time t1 of the first snapshot, snap1, the number n2 of
%   time steps from time t1 to  time t2 of the second snapshot, etc., up to 
%   n4. Example: If n1 = 20, n2 = 20, n3 = 10, n4 = 15, the snap-
%   shots are taken at times t1 = 1, t2 = 2, t3 = 2.5, and t4 = 3.25.
%      User must provide a function file heatf.m for the initial data
%   f(x). The function must be array smart.
%   User must also provide function files left.m and right.m for the
%   values u(0,t) and u(10,t). The boundary data functions do
%   not need to be array smart.





disp('   ')
k = input( 'enter the value of the diffusion coefficient k   ')
disp(' Enter the number of timesteps between snapshots ')
n = input (' In the form [n1, n2, n3, n4 ]           ')
delt = .05

n1 = n(1); n2= n(2); n3 = n(3); n4 = n(4);
t1 = n1*delt
t2 = t1 +n2*delt
t3 = t2 +n3*delt
t4 = t3 +n4*delt

delx = .05
r = .5*k*delt/(delx^2);

J= 10/delx;

x = 0:delx:10;

    snap0 = heatf(x);

v = snap0(2:1:J);

D = sparse(1:J-1, 1:J-1,(1+2*r), J-1, J-1, J-1);
E = sparse(2:J-1, 1:J-2, -r, J-1, J-1, J-1);
A = D + E +E';

[L,U] = lu(A);


% Note: the original index j runs from j = 1 ( x = 0) to j = J ( x = 10 -delx).
% The index in v (and hence in b and z) runs from j = 1 (x= delx) to
% j = J-1 (x = 10-delx).
for n = 1:n1
    b = 2*v ;
    b(1) = b(1) + r*( left(n*delt) +left((n-1)*delt) );
    b(J-1) = b(J-1) + r*( right(n*delt) +right((n-1)*delt) ) ;
    y = L\b';
    z = (U\y)';
    v = z- v;
end 

snap1 = [left(n*delt),v,right(n*delt)];
disp('Computed up to time t1')

for n = n1+1: n1+n2
    b = 2*v;
    b(1) = b(1) + r*( left(n*delt) + left((n+1)*delt) );
    b(J-1) = b(J-1) + r*( right(n*delt) + right((n+1)*delt) );
    y = L\b';
    z = (U\y)';
    v = z-v;
end

disp('Computed up to time t2 ')
snap2 = [left(n*delt),v,right(n*delt)];

for n = n1+n2+1: n1 + n2 +n3
    b = 2*v;
    b(1) = b(1) +r*( left(n*delt) + left((n+1)*delt) );
    b(J-1) = b(J-1) + r*( right(n*delt) + right((n+1)*delt) );
    y = L\b';
    z = (U\y)';
    v = z-v;
end

disp('Computed up to time t3 ')
snap3 = [left(n*delt),v,right(n*delt)];

for n = n1 + n2 +n3+1: n1+n2+n3+n4
    b = 2*v;
    b(1) = b(1) +r*( left(n*delt) + left((n+1)*delt) );
    b(J-1) = b(J-1) + r*( right(n*delt) + right((n+1)*delt) );
    y = L\b';
    z = (U\y)';
    v = z-v;
end

disp('Computed up to time t4 ')
snap4 = [left(n*delt),v,right(n*delt)];

plot(x,snap0,x,snap1,x,snap2,x,snap3,x,snap4)
