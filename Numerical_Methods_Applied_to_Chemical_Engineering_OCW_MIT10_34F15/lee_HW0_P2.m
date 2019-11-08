function lee_HW0_P2
% This function approximates Bessel functions of the first kind using a
% truncated series expansion and plots several traces of converging Bessels.
% Created September 15, 2010 Jason Moore
% Modified 9/8/2013 Mark Molaro
% Modified by 9/5/2914 Liza Lee
% Inputs: (none)
% Outputs: (generates a figure of Bessel functions)

rel_tol = true; %(optional) boolean for using relative tolerance (otherwise it is abs. tol)

% Testing my_bessel
[J,k] = my_bessel(0, 0, 0.001,rel_tol);
% J = 1
% k = 1
[J,k] = my_bessel(0.5, 2, 0.001,rel_tol);
% J = 0.5130      sqrt(2/(pi*2))*sin(2) = 0.5130
% k = 4
[J,k] = my_bessel(-0.5, 3, 0.001,rel_tol);
% J = -0.4560     sqrt(2/(pi*3))*cos(3) = -0.4560
% k = 6
[J,k] = my_bessel(2, 11.6918, 0.001,rel_tol)

[J,k] = my_bessel(2, 11.6198, 0.001,rel_tol)
% J = -0.0166     besselj(2,11.6918) = -0.0167
% k = 16

% Call plotting function
nu = 0;
r_max = 30;

tolvec = [linspace(0.5,0.01,5),0.001];
plot_bessel(nu,r_max,tolvec, rel_tol)

% /////////////////////////////////////////////////////////////////////////

function plot_bessel(nu, r_max,tolvec, rel_tol)
% Calculates the value of the Bessel function of the first kind by calling
% my_bessel, which takes the sum of the Bessel series
% September 15, 2010
% Jason Moore
% Modified by 9/5/2914 Liza Lee
% input variables:
%   nu - the nu variable of the Bessel function of the first kind
%   r_max - endpoint for the Bessel function
%   tolvec - vector of tolerances to be used to calculate Bessels (you
%   probably can't really pluralize Bessel function like this...)
%   tol_type - (optioanl) boolean for using relative tolerance (otherwise
%   absolute)
% output variables: (generates a figure of Bessel functions)

% Generate the vector of r values from 0 to r_max
r = linspace(0, r_max, 50)';

% Nested for loops to run through r and tol values
for j = 1:length(tolvec)+1
    for i = 1:50
        % For the last column of the matrix, calculates using Matlab's
        % besselj
        if j == length(tolvec)+1
            y(i,j) = besselj(nu, r(i));
        else
            [y(i,j),k(i,j)] = my_bessel(nu, r(i),tolvec(j),rel_tol);
        end
    end
end

% Define colormap for automatic change in line colors for multiple lines
num_lines = length(tolvec);
cc = lines(num_lines+1); % lines is one of the MATLAB built-in colormaps

% Plot bessel functions 
figure
Fontsize = 14; % Fontsize for the axis labels
set(gca,'NextPlot','replacechildren','ColorOrder',cc) % plots each line 
% (r vs. each column of y) with different color given in the colormap cc
plot(r,y,'Linewidth',1.5)
title('Bessel Function Using Finite Series','Fontsize',Fontsize);
xlabel('r','Fontsize',Fontsize);
ylabel('y(r)','Fontsize',Fontsize);
for j = 1:length(tolvec)
    legendvec{j} = ['tol = ',num2str(tolvec(j))];
end
legend([legendvec,'besselj'])

% Plot number of iterations required (optional), using hold and colormap, cc, for
% automatic plotting of lines
figure
hold on
for j = 1:num_lines
    plot(r,k(:,j),'Linewidth',1.5,'color',cc(j,:))
end
hold off
title('Bessel Function Using Finite Series','Fontsize',Fontsize);
xlabel('r','Fontsize',Fontsize);
ylabel('Number of Iterations','Fontsize',Fontsize);
legend([legendvec],'Location','SouthEast')

% /////////////////////////////////////////////////////////////////////////

function [J,k] = my_bessel(nu, r, tol, rel_tol)
% calculates the value of the Bessel function of the first kind by taking
% the sum of the Bessel series
% Created September 15, 2010 by Jason Moore
% Vectorization by Mark Molaro 9/8/2013
% Added option for rel vs. abs. tolerance by Liza Lee 9/5/2013
% input variables:
%   nu - the nu variable of the Bessel function of the first kind
%   r - the r value of the Bessel function at the current point
%     - can be a scalar or a vector for multiple evaluations 
%   tol - tolerance on Bessel function approximation (-)
%   rel_tol - (optioanl) boolean for using relative tolerance (otherwise
%   absolute)
% output variables:
%
%   J - the value of the Bessel function at the current point
%     - will be a scalar or a vector corresponding to the number of input points given 
%   k - the number of iterations required to converge (not required)

% If nu is a negative integer, generates an error message and terminates
if nu < 0 && rem(nu,1) == 0
    error('Error. Error. Input nu is a negative integer. Does not compute.')
end
%check inputs for vectorized computing
if size(r,2)>1
    error('r must be a scalar or column vector');
end
% Initialize variables

J = ones(size(r)); %J is the same size as r 
n = 0;
t = [];
k = 0;

% For loop to calculate Bessel function 
for k = 0:1000
    t(:,k+1) = (-1)^k.*(r/2).^(nu+2*k)./(factorial(k)*gamma(nu+k+1));
   
    % Checks for convergence of slowest converging element
    if rel_tol 
        c = max(abs(t(:,k+1)./(J))); % For relative tolerance
    else
        c = max(abs(t(:,k+1))); % For absolute tolerance
    end
    
    if c <tol && k > 0  
      break
    end
    J = sum(t,2); %sums along rows
end
