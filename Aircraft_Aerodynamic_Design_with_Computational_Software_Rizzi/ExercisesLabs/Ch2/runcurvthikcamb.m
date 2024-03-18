close all
clear all
format compact
homedir = pwd;
ifilsep = strfind(homedir, filesep);
libdir = [homedir filesep 'Ch2lib'];
rmpath(libdir);
addpath(libdir);
foil = 'naca4508'
xy   = foilfileread(foil);
figure(1)
hold on
plot(xy(:,1),xy(:,2),'k','linewidth',2)
dbg = 1
n   = 201
[xyc,thick,xyu,xyl,curv] = curvthikcamb(foil,n,dbg);
figure(1)
hold on
plot(xyc(:,1),xyc(:,2),'--k','linewidth',2)
plot(xyc(:,1),thick,':r','linewidth',2)
legend('foil','camber','thick')
title(foil)
set(gca,'fontsize',16)
drawnow
