%SSA_PLOT.M
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
%
% This is an extended version of ssa_mm.m, that 
% produces graphical output. There is an accompanying paper
% to appear in SIAM Review, Education section.
%
% D J Higham, 2006.

clf

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

count = 1;
tvals(1) = 0;
Xvals(:,1) = X;

while t < tfinal
     a(1) = c(1)*X(1)*X(2);
     a(2) = c(2)*X(3);
     a(3) = c(3)*X(3);
     asum = sum(a);
     j = min(find(rand<cumsum(a/asum)));
     tau = log(1/rand)/asum;
     X = X + V(:,j);
    
     count = count + 1;
     t = t + tau;
     tvals(count) = t;
     Xvals(:,count) = X;
end

%%%%%%%%%%% Plots

L = length(tvals);
tnew = zeros(1,2*(L-1));
tnew(1:2:end-1) = tvals(2:end);
tnew(2:2:end) = tvals(2:end);
tnew = [tvals(1),tnew];

Svals = Xvals(1,:);
ynew = zeros(1,2*L-1);
ynew(1:2:end) = Svals;
ynew(2:2:end-1) = Svals(1:end-1);
plot(tnew,ynew,'go-')
hold on

Pvals = Xvals(4,:);
ynew = zeros(1,2*L-1);
ynew(1:2:end) = Pvals;
ynew(2:2:end-1) = Pvals(1:end-1);
plot(tnew,ynew,'r*-')

text(40,240,'Product','FontSize',16)
text(30,50,'Substrate','FontSize',16)

xlabel('Time','FontSize',14)
ylabel('Molecules','FontSize',14)

axis([0 55 0 310])

set(gca,'FontWeight','Bold','FontSize',12)
grid on



