% Warblet
t0=5; w0=10; d=6; ma=2; mw=1; mf=0; %chirplet parameters
t=0:0.05:12; %times vector
g=exp(-(0.5/d)*((t-t0).^2));
insf=w0+(ma*sin((mw*(t-t0))+mf));
v=exp(-j*(insf.*(t-t0)));
h=(1/((pi*d)^0.25))*g.*v;

figure(1)
plot(t,real(h),'k');
title('warblet signal'); xlabel('sec');

figure(2)
plot(t,insf,'k')
title('instantaneous frequency of warblet signal');
xlabel('sec'); ylabel('rad/s');
