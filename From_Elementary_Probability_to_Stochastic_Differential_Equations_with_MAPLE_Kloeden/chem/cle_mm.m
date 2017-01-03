%CLE_MM.M
%
% Simple implementation of Euler-Maruyama to simulate the 
% Chemical Langevin Equation for the Michaelis-Menten system.
%
% Parameters from Chapter 7 of 
%      Stochastic Modelling for Systems Biology,
%      by Darren J. Wilkinson, Chapman & Hall/CRC, 2006.
%
% Downloadable from 
%    http://www.maths.strath.ac.uk/~aas96106/algfiles.html
% along with an extended version that produces graphical output.

randn('state',100)

%stoichiometric matrix
V = [-1 1 0; -1 1 1; 1 -1 -1; 0 0 1];

%%%%%%%%%% Parameters and Initial Conditions %%%%%%%%%
nA = 6.023e23;             % Avagadro's number
vol = 1e-15;               % volume of system
Y = zeros(4,1);
Y(1) = round(5e-7*nA*vol); % molecules of substrate
Y(2) = round(2e-7*nA*vol); % molecules of enzyme 
c(1) = 1e6/(nA*vol); c(2) = 1e-4; c(3) = 0.1;

tfinal = 50;
L = 250;
tau = tfinal/L;    % stepsize

for k = 1:L
     a(1) = c(1)*Y(1)*Y(2);
     a(2) = c(2)*Y(3);
     a(3) = c(3)*Y(3);
     d(1) = tau*a(1) + sqrt(abs(tau*a(1)))*randn;
     d(2) = tau*a(2) + sqrt(abs(tau*a(2)))*randn;
     d(3) = tau*a(3) + sqrt(abs(tau*a(3)))*randn;
     Y = Y + d(1)*V(:,1) + d(2)*V(:,2) + d(3)*V(:,3);

     % Record or plot system state here if required
end


