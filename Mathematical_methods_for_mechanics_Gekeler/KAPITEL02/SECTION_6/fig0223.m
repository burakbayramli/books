function fig0223
% Figure 2.23
clf
load datenc m X Periode
% -- X-Axis -------------
c  = 0.1;
d  = 0.04;
X1 = [-1.5, 1.5];
Y1 = [0,0];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Y-Axis --------------
c  = 0.1;
d  = 0.04;
X1 = [0,0];
Y1 = [-1,1];
arrow(X1,Y1,c,d,'k',2), hold on
% --Start trajectory ---------------------------
n         = 4;             % Dimension of system
m         = 200;           % number of subintervals
G         = 'bsp03';       % the current example
PeriodeA  = 6;             % estimated period
ANF       = [1.2;0;0;-1];  % estimated start value for AWP
mu        =  0.012128562765;   % problem-spezific parameter
Parmeter3 = [PeriodeA;mu];     % Parameter for example
TT        = linspace(0,1,m+1); % equidistant shooting points
%Parmeter2 = [n, m, bsp];  % Parameter for multiple shooting
Flag      = 1;
options   = odeset('reltol',1.0E-5,'abstol',1.0E-6);
[TA,X0]   = ode23(@bsp03,TT,ANF,options,Flag,Parmeter3);
Parmeter3 = Parmeter3(2);
X0        = X0';
plot(X0(1,:),X0(3,:),'g:','linewidth',2),hold on
plot(X(1,:),X(3,:),'k','linewidth',2), hold on
plot(X(1,:),X(3,:),'k.','markersize',12)
axis equal tight
Periode
% -- Checking of solution ------------------------
Periode   = 6.192169;
%ANF       = [1.2;0;0;-1.049357];
ANF       = X(:,1);
Parmeter3 = [Periode,mu];
[TA,XX]   = ode23(@bsp03,TT,ANF,options,Flag,Parmeter3);
XX        = XX';
plot(XX(1,:),XX(3,:),'b--','linewidth',2), hold on
%
circle(0,0,0.02,'w')
circle(-mu,0,0.02,'w')
circle(1-mu,0,0.025,'w')

grid off
xlabel('x');
ylabel('y');
%text(9.3,-0.25,'t','fontsize',24)
%text(7.3,-0.75,'x_1(t)','fontsize',24)
%text(5,1.25,'y_1(t)','fontsize',24)
%title('Arenstorf-Orbit I','fontsize',24)
