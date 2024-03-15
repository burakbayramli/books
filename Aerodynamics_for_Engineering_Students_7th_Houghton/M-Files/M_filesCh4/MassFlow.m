%
% Mass flow calculator for isentropic flow
% 
clear;clc;
M = .133;
Y = 1.4;
To = 15 + 273;
po = 101*1000;
R = 287;
D = 123/1000; % m
A = pi*D^2/4;%10/100/100; % Specified area in m^2
% Need this function in the workspace directory:
[Mstr,ppo,rro,TTo,AstrA] = Isentropic_FlowF(M,Y);
T = TTo*To;% K
p = ppo*po;% N/m/m
rhoo = po/(R*To);
rho = rro*rhoo; % kg/m/m/m
TstrTo = 2/(Y+1);
Tstr = TstrTo*To;
disp('[Mstr,ppo,rro,TTo,AstrA]')
[Mstr,ppo,rro,TTo,AstrA]
a = sqrt(Y*R*T);
aao = sqrt(TTo);
ao = a/aao;
astr = sqrt(Y*R*Tstr);
u = M*a; % m/s
Astr = AstrA*A;
mdot = rho*u*A
% The following formulas check
% % Shapiro's mass flow formula:
% fcw = (1 + ((Y-1)/2)*M^2)^((Y+1)/(2*(Y-1)));
% mdotS = A*M*sqrt(Y/R)*po/sqrt(To)/fcw
% % Liepmann & Roshko exercise 5.1 corrections:
% mdotLR = rhoo*ao*A*M*((1 + ((Y-1)/2)*M^2)^(-(Y+1)/(2*(Y-1))))
%
% The following occurs if Astr = A, i.e., the flow is 
% choked at the specified area which is assumed to be 
% the smallest area in the stream tube for the following 
% calculation:
% mdotmax = rhoo*ao*A*(2/(Y+1))^((Y+1)/(2*(Y-1)))
