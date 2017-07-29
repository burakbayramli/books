% Matlab file name:  standard_T_p.m
% Computes temperature, DT/dz for US standard atmosphere conditions
% from Z = 0 to 50 km geopotential height.
clear
close all
g = 9.81;
R = 287;
nl = 101;         % number of levels above ground
ztop = 50 ;       % upper boundary in km
nlm = nl -1;
dz = ztop/nlm;
T0 = 288.15;
z = linspace(dz,ztop,nlm);
p0=1013.25;
dTdz = zeros(size(z));
for n =1:length(z)
    if z(n) <= 11
        T(n) = 288.15 -6.5*z(n);
        dTdz(n) =  -6.5e-3;
    elseif z(n) > 11 & z(n) <= 20,  T(n) = 216.65;
    elseif z(n) > 20 & z(n) <= 32
        T(n) = 216.65 + (z(n) - 20);
        dTdz(n) = 1.e-3;
    elseif  z(n) > 32 & z(n) <= 47
        dTdz(n) = 2.8e-3;
        T(n) = 228.65 + 2.8*(z(n) - 32);
    elseif  z(n) > 47 & z(n) <= 51,  T(n) = 270.65;
    end
end
figure(1)
subplot(1,2,1);
plot([T0 T],[0 z]);
grid on
title('temperature'), xlabel('T  Kelvins'), ylabel ('height  km');
subplot(1,2,2);
plot(dTdz*1.e3,z);
grid on
title('lapse rate'),xlabel('dTdz    K/km'), ylabel('height km')







