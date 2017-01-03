%nm540
%to interpolate by Lagrange polynomial and get the derivative
%A ftn 'drvtp.m' to differentiate polynomial must be equipped.
clear, clf
x0=pi/4;  df0=cos(x0);
xx= [0: .01: pi];  yy= sin(xx);
for m=1:3
   if m==1, x= [1:3]*pi/8;
   elseif  m==2, x= [0:4]*pi/8; 
   else   x= [2:6]*pi/16; 
   end
   y=sin(x);
   subplot(220+m)
   plot(xx,yy,'k')
   hold on, plot(x,y,'r+')
   px= lagranp(x,y); %newtonp(x,y); 
   yy1= polyval(px,xx);
   plot(xx,yy1,'b')
   axis([0 pi 0 1.1])
   dfx= polyval(polyder(px), x0); %polyval(drvtp(px), x0);
   yy1= dfx*(xx-x0)+sin(x0); plot(xx,yy1,'k')
   fprintf(' dfx(%6.4f)=%10.6f (error:%10.6f)\n', x0, dfx, dfx-df0);
end
