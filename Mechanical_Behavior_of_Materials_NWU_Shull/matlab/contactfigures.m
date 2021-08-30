clear all; close all;
a=0.3;  % value of a/R for figure  (R is normalized to 1 here)
xrange=1.5;
yrange=0.3;

deltah=a^2;  % value of deltah/R

set(0,'defaultlinelinewidth',2)
set(0,'defaultaxesfontsize',16)
xsize=7;
ysize=4;
set(0,'defaultfigurepaperposition',[0,0,xsize,ysize])
set(0,'defaultfigurepapersize',[xsize,ysize]')
theta=linspace(0,2*pi,200);
xval=linspace(-xrange,xrange,2000);
xout=linspace(a,xrange,2000);
parabola=@(x) x.^2/2-1;
deltah=@(x) a^2;
hertzprofile=@(a,x) -1+deltah(a)-(deltah(a)/pi).*((2-(x./a).^2).*...
    asin(a./x)+(x./a).*((1-(a./x).^2).^0.5));
flatpunchprofile=@(deltaadh,a,x) -deltaadh+(2*deltaadh/pi)*asin(a./x);
jkrprofile=@(deltaadh,a,x) hertzprofile(a,x)+flatpunchprofile(deltaadh,a,x);
plot(cos(theta),sin(theta),'r-',xval,parabola(xval),'b-',...
    xout,hertzprofile(a,xout),'k-',...
    -xout,hertzprofile(a,xout),'k-');
set(gca,'visible','off')
xlim([-xrange xrange])
ylim([-1 -1+yrange])
legend('sphere','parabola','location','best')
hold on
arrow([-xrange,-1],[-xrange,deltah(a)-1],'ends',[1,2],'width',2)
text(-0.95*xrange,deltah(a)/2-1,'\delta_{h}','fontsize',16)
plot([-xrange,xrange],[deltah(a)-1,deltah(a)-1],'--k')
plot([-xrange,xrange],[-1,-1],'--k')
arrow([-a,a^2/2-1],[a,a^2/2-1],'ends',[1,2],'width',2)
text(0,a^2/1.4-1,'2a','fontsize',16,'horizontalalignment','center')
print('../contact_figures/hertzschematic','-depsc2')

%% now make the figure for the jkr profile
deltaadh=deltah(a);
figure 
plot(xval,parabola(xval),'b-',...
    xout,jkrprofile(deltaadh,a,xout),'k-',...
    -xout,jkrprofile(deltaadh,a,xout),'k-');
set(gca,'visible','off')
xlim([-xrange xrange])
ylim([-1 -1+yrange])
hold on
plot([-xrange,xrange],[-1,-1],'--k')
arrow([-a,a^2/2-1],[a,a^2/2-1],'ends',[1,2],'width',2)
text(0,a^2/1.4-1,'2a','fontsize',16,'horizontalalignment','center')

print('../contact_figures/jkrschematic','-depsc2')

% now do the flat punch situation
%%
xrange=5;
a=1;
xout=linspace(a,xrange,200);
deltaadh=0.1;  % 
figure
plot([-1,-1,1,1],[0.2,0,0,0.2],'b-')
hold on
xlim([-xrange xrange])
ylim([-0.15,0.25])

plot1=plot(xout,flatpunchprofile(deltaadh,a,xout),'-k',...
    -xout,flatpunchprofile(deltaadh,a,xout),'k-');
a=0.5;
xout=linspace(a,xrange,200);
plot2=plot(xout,flatpunchprofile(deltaadh,a,xout),'-r',...
    -xout,flatpunchprofile(deltaadh,a,xout),'r-');
arrow([-0.98*xrange,-deltaadh],[-0.98*xrange,0],'ends',[1,2],'width',2)
text(-0.95*xrange,-deltaadh/2,'\delta_{t}','fontsize',24)
plot([-xrange,xrange],[-deltaadh,-deltaadh],'--k')
plot([-xrange,-a],[0,0],'--k')
set(gca,'visible','off')
legend([plot1(1),plot2(1)],'a=a_{0}','a=0.5a_{0}','location','best')

arrow([-1,0.1],[1,0.1],'ends',[1,2],'width',2)
text(0,0.12,'2a_{0}','fontsize',16,'horizontalalignment','center')

% now we need to add the axis origin
arrow([0,-deltaadh],[1,-deltaadh],'ends',1,'width',2) 
text(0.5, -1.15*deltaadh,'r','fontsize',16,'horizontalalignment','center')

arrow([0,-deltaadh],[0,-0.5*deltaadh],'ends',1,'width',2)
text(-0.1, -0.85*deltaadh,'z','fontsize',16,'horizontalalignment','center')

print('../contact_figures/flatpunchschematic','-depsc2')

%%
figure
stress=@(r) 0.5*(1-r.^2).^-0.5;
stressk1=@(r) (1/(2*sqrt(2)))*(1./(1-r)).^0.5;
xvals=linspace(0,1,300);
plot(xvals,stress(xvals),'b-');
hold on
plot(xvals,stressk1(xvals),'r-');
xlabel('r/a')
ylabel('\sigma/\sigma_{avg}')
ylim([0,2])
legend('full solution','d^{-1/2} singularity', 'location','best')
print('../contact_figures/flatpunchstress','-depsc2')

