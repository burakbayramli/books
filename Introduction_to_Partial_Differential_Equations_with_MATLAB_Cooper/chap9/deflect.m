
%                    Program delect
%
%  This program computes the deflection of a membrane under a simple
%  load by integrating the Green's function over the unit disk against a 
%  function q(x,y). q is constant over a small disk with center (a,b)
%  and radius rho0. User must enter the values of a,b,rho, and q0. 
%  Output goes into the matrix U for plotting. The program also computes
%  the meshgrid for the unit disk using polar coordinates and plots
%  the solution. In addition it puts the values of the solution on the
%  x axis into the vector trace. The trace can be plotted with the
%  command plot(x,trace).


center = input('Enter the center of the load in the form [a,b]    ')
rho0 =   input('Enter the radius of the load   ')
q0 =     input('Enter the magnitude of the load   ')


r = 0: .05 : 1;
theta = 0: 2*pi/50 : 2*pi;

X = r'*cos(theta);
Y = r'*sin(theta);

a = center(1);
b = center(2);

delrho = rho0/10;
rho =  .5*delrho:delrho : rho0 - .5*delrho;

delphi = 2*pi/20;
phi = .5*delphi: delphi : 2*pi -.5*delphi;

xi = a +rho'*cos(phi);
eta= b +rho'*sin(phi);

z = zeros(size(X));

for k = 1:10
    for l = 1:20
        z= z + green(X,Y,xi(k,l), eta(k,l)).*rho(k);
     end
end
U = q0*rho0*delrho*delphi*z;

surf(X,Y,U)

x = -1: .05 : 1;
trace1 =  U(21:-1:2, 26)';
trace2 = U(1:21, 1)';
trace = [trace1, trace2];
