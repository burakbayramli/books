function sol = ch4ex1
global tau omega 
tau   = 42.0;
omega = 0.15;
sol = dde23(@ddes, [tau, omega], [15; 0; 2; 3], [0, 350]);
plot(sol.x,sol.y)
legend('S(t)', 'E(t)', 'I(t)', 'R(t)')
%==========================================
function dydt = ddes(t,y,Z)
global tau omega
% Parameters:
A = 0.330;
d = 0.006;
lambda = 0.308;
gamma = 0.040;
epsilon = 0.060;
% Variable names used in stating the DDEs:
S = y(1); E = y(2); I = y(3); R = y(4);
% Z(:,1) corresponds to the lag tau.
Itau = Z(3,1);
% Z(:,2) corresponds to the lag omega.
Somega = Z(1,2); Eomega = Z(2,2); Iomega = Z(3,2); Romega = Z(4,2);
Noft = S + E + I + R;
Nomega = Somega + Eomega + Iomega + Romega;
dSdt = A - d*S - lambda*((S*I)/Noft) + gamma*Itau*exp(-d*tau);
dEdt = lambda*((S*I)/Noft) - ...
       lambda*((Somega*Iomega)/Nomega)* exp(-d*omega) - d*E; 
dIdt = lambda*((Somega*Iomega)/Nomega)*exp(-d*omega) ...
       - (gamma+epsilon+d)*I;
dRdt = gamma*I - gamma*Itau*exp(-d*tau) - d*R;

dydt = [ dSdt; dEdt; dIdt; dRdt];