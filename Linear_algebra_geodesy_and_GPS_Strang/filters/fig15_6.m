%FIG15_6   Script for plotting receiver clock offsets
%	        for Turbo-SII and Z-12 receivers.
%          Produces Figure 15.6

%Kai Borre 03-18-97
%$Copyright (c) 1997 by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

v_light = 299792458;	      % vacuum speed of light m/s
OffSet = kalclock('ptb.96o','pta.nav',0);
fprintf('\nAdded %6.0f meters to obtain positive offsets\n',...
                                             -floor(min(OffSet)))
OffSet = OffSet - floor(min(OffSet));
t = 1:80;
z = 1:240;

%FIGURE PRODUCTION
%To get a sample of Z-12 clock offsets uncomment the third next line.
%It is only to be run once for getting the data into the workspace
%The computation lasts for one or two minutes!
b_clock('b0810a94.076')

fid = fopen('clock.dat','r');
[T,count] = fread(fid,inf,'double');
fprintf('\nAdded %6.0f meters to obtain positive offsets\n\n',...
                                             -floor(min(T(1:240))))
A = T(1:240)-floor(min(T(1:240)));
toc

figure;
semilogy(20*z, A/v_light,'b-',15*t,OffSet/v_light,'r--')
ylabel('Clock offset  [s]','Fontsize',16)
xlabel('Time  [s]','Fontsize',16)
set(gca,'FontSize',16)
hl = legend(' Clock with reset', ' Steered  clock ');
axes(hl);
set(gca,'FontSize',16)
disp('Use mouse to move the legend')
disp('Press ENTER to finish.')
refresh, pause
print fig15_6 -deps

fclose('all');
%%%%%%%%% end fig15_6.m %%%%%%%%%
