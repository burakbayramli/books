function fig0222
% Figure 2.22
clf
load datenb m X Periode
plot3(X(1,:),X(2,:),X(3,:),'k','linewidth',2), hold on
N = size(X,2);
M = [14,15,16,30,31,32];
plot3(X(1,M),X(2,M),X(3,M),'k.','markersize',12), hold on
Periode
%axis tight
axis([-60 60 -60 60 100 220])
grid on
xlabel('x','fontsize',22);
ylabel('y','fontsize',22);
zlabel('z','fontsize',22);

% -- Start trajectory ------------------------
Flag      = 1;
options   = odeset('reltol',1.0E-1,'abstol',1.0E-2);
TTA       = linspace(0,1,m+1);   % equidistant shooting points
Periode   = 0.95;         % estimated period
ANF       = [0;-28;140];  % estimated start value for AWP
sig       = 16;           % problem-spezific parameter
b         = 4;            % problem-specific parameter
r         = 153.083;      % problem-specific parameter
Parmeter3 = [Periode,sig,b,r]; % Parameter for example
[TA,XX]   = ode23(@bsp02,TTA,ANF,options,Flag,Parmeter3);
XX        = XX';
plot3(XX(1,:),XX(2,:),XX(3,:),'k:','linewidth',2)
view([30 30])

%title('Waermeleitproblem','fontsize',24)
