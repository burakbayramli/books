%SSA_MM.M
%
% Simple implementation of the Stochastic Simulation Algorithm
% (or Gillespie's algorithm) on the Michaelis-Menten system.
%
% Parameters from Chapter 7 of 
%      Stochastic Modelling for Systems Biology,
%      by Darren J. Wilkinson, Chapman & Hall/CRC, 2006.
%
% Downloadable from 
%    http://www.maths.strath.ac.uk/~aas96106/algfiles.html
% along with an extended version that produces graphical output.

rand('state',100)

%stoichiometric matrix
V = [-1 1 0; -1 1 1; 1 -1 -1; 0 0 1];

%%%%%%%%%% Parameters and Initial Conditions %%%%%%%%%
nA = 6.023e23;             % Avagadro's number
vol = 1e-15;               % volume of system
X = zeros(4,1);
X(1) = round(5e-7*nA*vol); % molecules of substrate
X(2) = round(2e-7*nA*vol); % molecules of enzyme 
c(1) = 1e6/(nA*vol); c(2) = 1e-4; c(3) = 0.1;

t = 0;
tfinal = 50;
while t < tfinal
     a(1) = c(1)*X(1)*X(2);
     a(2) = c(2)*X(3);
     a(3) = c(3)*X(3);
     asum = sum(a);
     j = min(find(rand<cumsum(a/asum)));
     tau = log(1/rand)/asum;
     X = X + V(:,j);
     t = t + tau;
    
     % Record or plot system state here if required
end


