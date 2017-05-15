%Propagation through nonlinearity
%
%nonlinear function
vx=-10:0.1:10;
vy=atan(vx);

%gaussian random data
Nd=40000;
sig=0.1;
adat=sig*randn(1,Nd);
%histogram of a priori data
Nbins=50;
[ha,hax]=hist(adat,Nbins); 

%propagated random data
pdat=atan(adat);
%histogram of posterior data
[hp,hpx]=hist(pdat,Nbins); 

figure(1)
plot(vx,vy);
title('arctan function');

figure(2)
plot(hax,ha,'k'); hold on;
plot(hpx,hp,'r');
title('propagation through arctan: PDFs');