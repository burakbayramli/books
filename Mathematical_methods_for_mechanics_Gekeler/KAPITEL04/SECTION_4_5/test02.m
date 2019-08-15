function test02
% Hoehenformel Hoehe h in km
clc, clf
rho_0 = 1.2255;
BETA = 14;
[altitude,sigma,temp] = atmos;
NN = length(altitude);
HH = altitude(16:20);
DRUCK1 = rho_0*exp(sigma(16:20));
plot(HH,DRUCK1,'k','linewidth',2), hold on
DRUCK2 = rho_0*exp(-BETA*HH*1E-5);
plot(HH,DRUCK2,'r--','linewidth',2), hold on
BETA = 1.3976E-4;
rho_0 = 1.3932;
DRUCK3 = rho_0*exp(-BETA*HH);
plot(HH,DRUCK3,'b:','linewidth',2), hold on
xlabel(' altitude ','fontsize',10)
ylabel(' Luftdruck (kg/m^3)','fontsize',10)
legend(' gemessen','skaliert','berechnet')



%rho_1 = 2.704E-3;
%BTA = 4.26;
%DRUCK3 = rho_1*1E2*exp(-BTA*3.28*HH*1E-5);
%AA = DRUCK2(1)
%BB = DRUCK3(1)
%FAK = DRUCK2(1)/DRUCK3(1)
%plot(HH,DRUCK3,'b','linewidth',2), hold on


