function fig0221
% Figure 2.21
clf
load datena m X Periode
% -- X-Axis -------------
c  = 0.4;
d  = 0.05;
X1 = [-0.5, 10];
Y1 = [0,0];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Y-Axis --------------
c  = 0.2;
d  = 0.15;
X1 = [0,0];
Y1 = [-2,2];
arrow(X1,Y1,c,d,'k',2), hold on
% -- Axis of period -------
X1 = [Periode,Periode];
Y1 = [-2,2];
plot(X1,Y1,'--','color','k'), hold on
% -----------------------------
TT = linspace(0,Periode,m+1);
plot(TT,X(1,:),'-b',TT,X(1,:),'ob','linewidth',2), hold on
plot(TT,X(2,:),'-g',TT,X(2,:),'og','linewidth',2), hold on

Periode
axis tight
text(9.3,-0.25,'t','fontsize',24)
text(7.3,-0.75,'y_1(t)','fontsize',24)
text(5,1.25,'y_2(t)','fontsize',24)
text(0.2,-1.5,'-- x(t)','fontsize',24)
text(0.2,-0.7,'... y^0(t)','fontsize',24)

Flag      = 1;
options   = odeset('reltol',1.0E-1,'abstol',1.0E-2);
TTA       = linspace(0,1,m+1);   % equidistant shooting points
% -- Starttrajektorie --------------------------
PeriodeA  = 12;
ANF       = [3;1.5];
Parmeter3 = [PeriodeA];    % Parameter for example
[TA,XX]   = ode23(@bsp01,TTA,ANF,options,Flag,Parmeter3);
XX        = XX';
plot(TT,XX(1,:),'b:','linewidth',2), hold on
plot(TT,XX(2,:),'g:','linewidth',2), hold on
% -- Checking of solution -------------------------
ANF       = X(:,1);
Parmeter3 = Periode;
[TA,XX]   = ode23(@bsp01,TTA,ANF,options,Flag,Parmeter3);
Parmeter3 = [PeriodeA];    % Parameter for example
XX        = XX';
plot(TT,XX(1,:),'b--','linewidth',2), hold on
plot(TT,XX(2,:),'g--','linewidth',2), hold on
%title('Nervenmembranmodell','fontsize',24)
