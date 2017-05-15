% Gaussian chirplet
t0=5; w0=10; d=6; c=2; %chirplet parameters
t=0:0.05:12; %times vector
g=exp(-(0.5/d)*((t-t0).^2));
v=exp(-j*(w0+((0.5*c)*(t-t0))).*(t-t0));
h=(1/((pi*d)^0.25))*g.*v;

plot(t,real(h),'k');
title('Gaussian chirplet signal'); xlabel('sec');
