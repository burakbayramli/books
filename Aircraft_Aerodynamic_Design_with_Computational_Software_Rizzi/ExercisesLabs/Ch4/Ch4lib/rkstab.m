% RK stab
%close all
%clear all
format compact
RKcoeffs ={[2/3 2/3 1],[0.0695  0.1602  0.2898 0.5060  1.0000 ]};
nms = {'Edge','DEMOFLOW'};
nm = length (RKcoeffs);
xmin = -7; xmax = 1; ymin = -5; ymax = 5;
np = 200;
[xx,yy] = meshgrid(linspace(xmin,xmax,np),linspace(ymin,ymax,np));
z = xx+1i*yy;
zone = ones(size(xx));
figure(1)
hold on
for k =2
    c = RKcoeffs{k}
    p = zone;
    ns = length (c);
    for m = 1:ns
        p = zone+c(m)*z.*p;
    end
    %subplot(1,nm,k)
    contour(xx,yy,abs(p),linspace(0,1,7),'k')
    hold on
    plot([xmin,xmax],[0 0],'k')
    plot([0 0],[ymin ymax],'k')
    title(nms{k},'fontsize',14)
    axis([xmin-0.1 xmax+0.1 0 ymax+0.1])
    %axis equal
end