% Dopplerlet
t0=0.3; d=0.01; landa=0.1; r0=10; L0=1000; v0=500; ac=-20; %chirplet parameters
t=0:0.005:0.8; %times vector
g=exp(-(0.5/d)*((t-t0).^2));
mx=L0-((v0*t)+(0.5*ac*(t.^2)));
me=sqrt(r0^2+(mx.^2));
v=exp(-j*(2/landa)*me);
h=(1/((pi*d)^0.25))*g.*v;

figure(1)
plot(t,real(h),'k');
title('dopplerlet signal'); xlabel('sec');

