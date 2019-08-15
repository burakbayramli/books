function void = ch1ex6(void)
% Lapidus et alia, proton transfer
% x1 = y(1), x2 = y(2), y = y(3)
tspan = [0 8e5];
y0 = [0; 1; 0];
opts = odeset('AbsTol',1e-20);
[t,y] = ode15s(@f,[0 8e5],y0,opts);
%plot(t,y(:,1),t,y(:,2))
plot(t,y(:,1),'-k',t,y(:,2),':ko','MarkerSize',2,'MarkerFaceColor','k')
%print -depsc ch1fig6a
figure
%semilogx(t,y(:,3))
semilogx(t,y(:,3),'-k')
%print -depsc ch1fig6b
%===================================================
function dydt = f(t,y)
k = [8.4303270e-10 2.9002673e+11 ... 
	 2.4603642e+10 8.7600580e-06];

dydt = zeros(3,1);
dydt(1) = -k(1)*y(1) + k(2)*y(3);
dydt(2) = -k(4)*y(2) + k(3)*y(3);
dydt(3) = k(1)*y(1) + k(4)*y(2) - (k(2) + k(3))*y(3);