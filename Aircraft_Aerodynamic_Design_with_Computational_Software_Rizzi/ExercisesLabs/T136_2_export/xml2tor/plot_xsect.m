function plot_xsect(x,z,rz,ry)
n = 20;
fi = linspace(0,2*pi,n);
xpl = x*ones(n,1);
ypl = 0+ry*sin(fi);
zpl = z + rz*cos(fi);
plot3(xpl,ypl,zpl,'k')