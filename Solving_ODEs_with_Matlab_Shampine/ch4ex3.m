function sol = ch4ex3
global c D G n
c = 0.5; D = 5; G = 1; n = 100; 
% x = y(1), y = y(2), lambda = y(3), 
% I_1 = y(4), I_2 = y(5), I_3 = y(6).
y0 = [0.8*n; 0.2*n; 0; 0; 0; 0];
sol = dde23(@odes,[],y0,[0, D]);
sol = dde23(@ddes,D,sol,[D, 4*D]);
plot(sol.x,[sol.y(1:2,:); 100*sol.y(3,:)]);
legend('x(t)','y(t)','100 \lambda(t)',0)
%============================================================
function dydt = odes(t,y,Z)
global c D G n
dydt = zeros(6,1);
dydt(1) = - y(1)*y(3) + G*y(2);
dydt(2) = - dydt(1);
dydt(4) = exp(G*t)*y(2);
dydt(5) = t*exp(G*t)*y(1)*y(3);
dydt(6) = exp(G*t)*y(1)*y(3);
dydt(3) = (c/n)*exp(-G*t)*((dydt(4)+dydt(5))-G*(y(4)+y(5)));

function dydt = ddes(t,y,Z)
global c D G n
dydt = zeros(6,1);
dydt(1) = - y(1)*y(3) + G*y(2);
dydt(2) = - dydt(1);
dydt(4) = exp(G*t)*y(2) - exp(G*(t - D))*Z(2);
dydt(5) = D*exp(G*t)*y(1)*y(3) - y(6);
dydt(6) = exp(G*t)*y(1)*y(3) - exp(G*(t - D))*Z(1)*Z(3);
dydt(3) = (c/n)*exp(-G*t)*((dydt(4)+dydt(5))-G*(y(4)+y(5)));