% Display of Mexican hat wavelet
t=-7:0.01:7; 
C=2/(sqrt(3)*sqrt(sqrt(pi)));
psi=C*(1-t.^2).*exp(-0.5*t.^2);

plot(t,psi,'k'); %plots the wavelet
axis([-7 7 -0.5 1]);
title('Mexican hat wavelet');
