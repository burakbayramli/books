% P 4.1:
clear;clc
D = 150/1000; % m
Am = pi*D^2/4;
% (a)
% pm = 101000; % N/m/m
% rhom = 1.2256; % kg/m/m/m
% R = 0.286;
% Tm = pm/R/rhom; % K
% dp = (127/1000) * 1000*9.81; % N/m/m, dp = (po - p2) = po*(1-p2po)
% po = 1.009*pm;
% p2 = po - dp;
% ppo = p2/po
% Y=1.4;
% AAstrt = (1/ppo)^(1/Y) * sqrt( ((Y-1)/2)*(2/(Y+1))^((Y+1)/(Y-1)) ...
%    * 1/( 1 - (ppo)^((Y-1)/Y) ) );
% ppo1 = pm/po;
% AAstrm = (1/ppo1)^(1/Y) * sqrt( ((Y-1)/2)*(2/(Y+1))^((Y+1)/(Y-1)) ...
%    * 1/( 1 - (ppo1)^((Y-1)/Y) ) );
% AmAt = AAstrm/AAstrt;
% Dt = sqrt(Am/AmAt)*1000
% (b)
pm = 100300; % N/m/m
R = 0.286;
Tm = 100 + 273;  % K
rhom = pm/R/Tm;
dp = (127/1000) * 13534*9.81; % N/m/m, dp = (po - p2) = po*(1-p2po)
po = 1.009*pm;
p2 = po - dp;
ppo = p2/po
Y=1.4;
AAstrt = (1/ppo)^(1/Y) * sqrt( ((Y-1)/2)*(2/(Y+1))^((Y+1)/(Y-1)) ...
   * 1/( 1 - (ppo)^((Y-1)/Y) ) );
ppo1 = pm/po;
AAstrm = (1/ppo1)^(1/Y) * sqrt( ((Y-1)/2)*(2/(Y+1))^((Y+1)/(Y-1)) ...
   * 1/( 1 - (ppo1)^((Y-1)/Y) ) );
AmAt = AAstrm/AAstrt;
Dt = sqrt(Am/AmAt)*1000

