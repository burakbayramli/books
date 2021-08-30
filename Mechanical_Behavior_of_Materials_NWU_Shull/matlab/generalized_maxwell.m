clear all;  close all; % reset everything in MATLAB
% now we do a bunch of stuff to make the plot pretty
set(0,'defaultlinelinewidth',2)
set(0,'defaultaxesfontsize',16)
set(0,'defaultfigurepaperposition',[0,0,7,5])
set(0,'defaultfigurepapersize',[7,5]')

% now we enter the relaxation times and normalized moduli
gr=1e6;  % relaxed modulus
g0=1e9;  % unrelaxed modulus
logtau=[-7,-6.5,-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,0,...
    0.5,1,1.5,2,2.5,3,3.5,4,5];
tau=10.^[logtau]; 
g=[0.0215,0.0215,0.0215,0.0215,0.0267,0.0267,0.0375,0.0405,0.0630,0.0630,...
    0.1054,0.1160,0.1160,0.1653,0.0561,0.0561,0.0199,0.0119,0.0055,...
    0.0028,0.0008,0.0002,0.0003,0.0003];

% now we define the various functions that we'll need to plot
% note that 'sum' just adds up all the components of an array - it's used
% here so we don't need to create a loop to add everything up

gt=@(t) gr+(g0-gr)*sum(g.*exp(-t./tau));  % this is the time-dependent relaxation modulus
gstar=@(w) gr+(g0-gr)*sum(g./(1-1i./(w*tau)));    % this is the complex, frequency dependent modulus
gmag=@(w) abs(gstar(w));  % magnitude of the complex modulus
gstor=@(w) real(gstar(w));  % storage modulus
gloss=@(w) imag(gstar(w));  % loss modulus
phi=@(w) atand(gloss(w)/gstor(w));  % phase angle in degrees

% determine plot range that spans the full range of tau values
% use logspace to create points equally spaced on a log scale
% (these can also just be entered manually, but these values give pltos
% that look good)
tplot=logspace(min(logtau)-1,max(logtau)+1);
wplot=logspace(-max(logtau)-1,-min(logtau)+1);

% now make the plots - use 'arrayfun' to evaluate a function
% at all the points defined by an array
% start with the relaxation modulus plot
loglog(tplot,arrayfun(gt,tplot),'b-');
xlabel('t (s)')
ylabel('G(t) (Pa)')
ylim([0.5*gr,2*g0])
xlim([min(tplot),max(tplot)])
print('../solution_figures/gtplot','-depsc2')

% now make the gstar vs. freq. plot
figure  % creates a new figure
loglog(wplot,arrayfun(gmag,wplot),'b-');
xlabel('\omega (s^{-1})')
ylabel('|G^{*}(\omega)| (Pa)')
ylim([0.5*gr,2*g0])
xlim([min(wplot),max(wplot)])
print('../solution_figures/gwplot','-depsc2')

% now make the gstar vs. phase angle plot
figure  % creates a new figure
semilogy(arrayfun(phi,wplot),arrayfun(gmag,wplot),'b-');
xlabel('\phi (deg.)')
ylabel('|G^{*}(\omega)| (Pa)')
ylim([0.5*gr,2*g0])
xlim([0, 60])
print('../solution_figures/gphiplotplot','-depsc2')







