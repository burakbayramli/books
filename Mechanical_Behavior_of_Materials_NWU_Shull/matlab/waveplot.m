clear all;  close all; % reset everything in MATLAB
% now we do a bunch of stuff to make the plot pretty
set(0,'defaultlinelinewidth',2)
set(0,'defaultaxesfontsize',16)
set(0,'defaultfigurepaperposition',[0,0,7,5])
set(0,'defaultfigurepapersize',[7,5]')
% now we define the variables
f=5e6;  % frequency
gmag=1e8;  phi=20;  rho=1000; % define material props.
phir=pi*phi/180;  % convert to radians
gstar=gmag*exp(1i*phir);  % define complex g
kstar=2*pi*f*sqrt(rho/gstar);
ustar=@(z,t) exp(1i*(2*pi*f*t-kstar*z)); % this is how functions are written
ut0=@(z) real(ustar(z,0));
ezplot(ut0,[0,1e-4]) % easiest way to plot a one-dimensional function
xlabel('z (m)')
ylabel('normalized u_{x}')
title(['|G*|=',num2str(gmag,'%6.2e'),', \phi=',num2str(phi),'^{\circ}']);
print('../figures/waveplot.eps', '-depsc2') % save the plot as an eps file