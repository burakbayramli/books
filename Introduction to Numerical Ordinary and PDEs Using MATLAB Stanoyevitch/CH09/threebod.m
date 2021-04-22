function xp=threeb(t,xv)
x=xv(1);, z=xv(2);, y=xv(3);, w=xv(4);
xm=1/82.45;, xe=1-xm;, dm=((x+xm)^2+y^2)^(1/2);, de=((x-xe)^2+y^2)^(1/2);
xp(1)=z;
xp(2)=2*w+x-xe*(x+xm)/dm^3-xm*(x-xe)/de^3;
xp(3)=w;
xp(4)=-2*z+y-xe*y/dm^3-xm*y/de^3;
xp=xp';

