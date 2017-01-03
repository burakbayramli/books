function demoThreeRes(z,maxit)
% demoThreeRes  Solve the three reservoir problem with Newton's method
%
% Synopsis:  demoThreeRes(z,maxit)
%
% Input:  z = (optional) vector of four elements containing the elevation
%              of each of the three reservoirs (z(1:3)) and the elevation
%              of the junction, z(4).  Default:  z = [30; 18; 9; 11] (m).
%         maxit = (optional) max number of iterations.  Default: maxit = 5
%
% Output:  Solution is printed to command window

if nargin<1,  z = [30; 18; 9; 11];  end
if nargin<2,  maxit=15;             end

% --- Assign problem parameters
L = [3000; 600; 1000];           %  Lengths of pipes, (m)
d = [1; 0.45; 0.6];              %  diameters of pipes,  (m)
area = 0.25*pi*d.^2;             %  area of pipe sections
Kf = [0.015; 0.024; 0.020];      %  friction factor,  assumed constant
c = Kf.*(L./d)./(2*9.8*area.^2); %  constant in head loss formula
q  = [1; 1; 1];                  %  intial guess at flow rates,  (m^3/s)
pj = 1e5;                        %  initial guess at junction pressure, (Pa)

% --- Solve the system and print results
x0 = [q; pj];                   %  initial guess at solution vector
x = newtonSys('JfReservoir',x0,[],[],maxit,1,z,c);

fprintf('\nFlows rates: Q1 = %5.2f, Q2 = %5.2f, Q3 = %5.2f m^3/s\n',x(1:3));
fprintf('Junction pressure = %8.0f Pa\n',x(4));
fprintf('Net flow rate into junction = %12.3e m^3/s\n',sum(x(1:3)));
