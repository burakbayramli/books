function[h1, h2, h3]=faeropolar(alpha,CL,CD,Cm,marker);

CDaxis=axes('position',[0.1 0.1 0.2 0.8]);
h1=plot(CD,CL,marker);
set(h1,'Linewidth',2);
xlabel('C_D, [-]');
ylabel('C_L, [-]');
grid;
hold on;

QQ=(get(CDaxis,'XTick'));
set(CDaxis,'XTick',QQ(1:end-1));

CLaxis=axes('position',[0.3 0.1 0.5 0.8]);
h2=plot(alpha,CL,marker);
set(h2,'Linewidth',2);
xlabel('Angle of attack, \alpha, [deg] ');
grid;
title('Polar and Liftcurve.');
hold on;

QQ=(get(CLaxis,'XTick'));
set(CLaxis,'XTick',QQ(1:end-1));
set(CLaxis,'YTickLabel',' ');

Cmaxis=axes('position',[0.8 0.1 0.15 0.8]);
h3=plot(Cm,CL,marker);
set(h3,'Linewidth',2);
xlabel('C_m, [-]');
grid;
set(Cmaxis,'YTickLabel',' ');
hold on;





