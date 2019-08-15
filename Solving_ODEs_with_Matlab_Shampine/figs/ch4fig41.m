function sol = ch4ex1
global A d lambda gamma epsilon tau omega
A       =  0.330;
d       =  0.006;
lambda  =  0.308;
gamma   =  0.040;
epsilon =  0.060;
tau   = 42.0;
omega = 0.15;
sol = dde23(@ddes, [tau, omega], [15; 0; 2; 3], [0, 350]);
%plot(sol.x, sol.y(1,:), sol.x, sol.y(2,:), ...
%     sol.x, sol.y(3,:), sol.x, sol.y(4,:));
plot(sol.x, sol.y(1,:), ':ko' , sol.x, sol.y(2,:), '-k', ...
     sol.x, sol.y(3,:), '--k', sol.x, sol.y(4,:), '-.k', ...
     'MarkerSize',2);
legend('S(t)', 'E(t)', 'I(t)', 'R(t)')
%print -depsc ch4fig1
%==========================================
function dydt = ddes(t,y,Z)
global A d lambda gamma epsilon tau omega
S = y(1);
E = y(2);
I = y(3);
R = y(4);
Noft = S + E + I + R;
ylag1 = Z(:,1);
ylag2 = Z(:,2);
Itau   = ylag1(3);
Somega = ylag2(1);
Eomega = ylag2(2);
Iomega = ylag2(3);
Romega = ylag2(4);
Nomega = Somega + Eomega + Iomega + Romega;
dSdt = A - d*S - lambda*((S*I)/Noft) + gamma*Itau*exp(-d*tau);
dEdt = lambda*((S*I)/Noft) - ...
       lambda*((Somega*Iomega)/Nomega)* exp(-d*omega) - d*E;   
dIdt = lambda*((Somega*Iomega)/Nomega)*exp(-d*omega) ...
       - (gamma+epsilon+d)*I;
dRdt = gamma*I - gamma*Itau*exp(-d*tau) - d*R;
dydt = [ dSdt; dEdt; dIdt; dRdt];