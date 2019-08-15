function surface_wave
% simulation of a solitary wave after Zienkiewicz II, pp. 187/188

clc, format short, format compact

length = 16; d = 1; H = 0.3*d; c = 1; g = 9.81;

c = 1 + 0.5*H/d - (3/20)*(H/d)^2;
c = c*sqrt(g*d);

t = 0; DT = 0.25; TMAX = 100;

XX1 = linspace(0,length,40);
AUX1 = sqrt(3*H/(4*d^3));
clf
plot(0,0,'w.','markersize',6), hold on
plot(16,6,'w.','markersize',6), hold on
axis equal tight, axis manual

for I = 1:TMAX
   AUX  = AUX1*(XX1 - c*t);
   h = d + H*sech(AUX).^2;
   t = t + DT;
   pp = plot(XX1,3*h);
   pause(0.2)
   delete(pp)
end 
