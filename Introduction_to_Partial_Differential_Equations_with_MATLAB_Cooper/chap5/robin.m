

%                          Program Robin
%
%    Solves u-tt -u_xx= 0 on the half line c > 0 with the Robin
%    boundary condition  u_x  - hu = 0   at x = 0 for an 
%    incident wave form G(x+ t).  The program assumes there is
%    a function file bigg.m for the function G(x).
%         To run the program, user must enter the times 
%    t1, t2, t3, t4 of the snapshots snap1, snap2, snap3, and snap4. 
%    snap0 is the initial profile G(x). Plots are done on [0,10].
%    User must also enter the value of h.



x = 0:.1:10;

snap0 = bigg(x);
t = input('  Enter the times in the form [t1,t2,t3,t4]           ')

t1 = t(1) ;  t2 = t(2);  t3 = t(3);  t4 = t(4);
disp('   ')

h = input('Enter the value of h   ')
% midpoints of the x intervals
y = .05:.1:9.95;


%calculation of the integral term
% first column of the matrix - values of the integrand at the midpoints
w = (exp(-h*y).*bigg(t1-y))';
% makes a square matrix a having all columns equal to w 
a = w(:,ones(size(y')));
%lower triangular part for integrating from x to 10
b = tril(a);
%calculation of the integral by summing the columns of the matrix b
int = [.1*sum(b), 0];
snap1 = bigg(t1+x) + bigg(t1-x) -2*h*exp(h*x).*int;

w = (exp(-h*y).*bigg(t2-y))';
a = w(:, ones(size(y')));
b = tril(a);
int = [.1*sum(b),0];
snap2 = bigg(t2 +x) +bigg(t2-x) -2*h*exp(h*x).*int;

w = (exp(-h*y).*bigg(t3-y))';
a = w(:, ones(size(y')));
b = tril(a);
int = [.1*sum(b),0];
snap3 = bigg(t3 +x) + bigg(t3-x) -2*h*exp(h*x).*int;

w = (exp(-h*y).*bigg(t4 -y))';
a = w(:,ones(size(y')));
b = tril(a);
int = [.1*sum(b),0];
snap4 = bigg(t4+x) + bigg(t4-x) -2*h*exp(h*x).*int;

end
plot(x,snap0,x,snap1,x,snap2, x,snap3,x,snap4)
