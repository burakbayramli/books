function ch2ex1
% x_1 = y(1), x_2 = y(2), y = y(3)
options = odeset('AbsTol',1e-20);
[t,y] = ode15s(@odes,[0 8e5],[0; 1; 0],options);
plot(t,y(:,1:2))
figure
semilogx(t,y(:,3))
%===================================================
function dydt = odes(t,y)
k = [8.4303270e-10 2.9002673e+11 2.4603642e+10 8.7600580e-06];
dydt = zeros(3,1);
dydt(1) = -k(1)*y(1) + k(2)*y(3);
dydt(2) = -k(4)*y(2) + k(3)*y(3);
dydt(3) =  k(1)*y(1) + k(4)*y(2) - (k(2) + k(3))*y(3);