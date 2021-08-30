clear all; close all;
set(0,'defaultlinelinewidth',2)
set(0,'defaultaxesfontsize',16)
set(0,'defaulttextfontsize',16)
xsize=7;
ysize=4;
set(0,'defaultfigurepaperposition',[0,0,xsize,ysize])
set(0,'defaultfigurepapersize',[xsize,ysize]')
xval=linspace(-5,5,300);
shear=@(x) x;
extension=@(x) x-1./x.^2;
plot(xval, shear(xval),'b-');
xlabel('\gamma_{xy} (shear strain)')
ylabel('\sigma_{xy}/G')
print('../fracture_figures/neohookian shear','-depsc2')

%%
figure
xval=linspace(0.2,5,300);
plot(xval, extension(xval),'b-');
xlabel('\lambda_{z}')
ylabel('\sigma_{zz}/G')
ylim([-5,5])
xlim([0,3])
hold on
plot([1,1],[-10,10],'k--')
text(0.5,4,'compression','horizontalalignment','center')
text(2,4,'extension','horizontalalignment','center')
print('../fracture_figures/neohookian extension','-depsc2')


