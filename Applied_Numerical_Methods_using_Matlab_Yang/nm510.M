%nm510: the volume of a sphere
x=[-1:0.05:1]; y=[0:0.05:1];
[X,Y]=meshgrid(x,y);
f59=inline('sqrt(max(1-x.*x-y.*y,0))','x','y');
Z=f59(X,Y); %*100;
colormap('gray')
mesh(x,y,Z);  
colormap('default')
a=-1; b=1;
c=0; d=inline('sqrt(max(1-x.*x,0))');
Vt= int2t(f59,a,b,c,d,100,100) %trapezoidal
error=Vt-pi/3
Vs1= int2s(f59,a,b,c,d,100,100) %Simpson
error1=Vs1-pi/3
Vs2= int2s(f59,a,b,c,d,0.01,0.01) %Simpson
error2=Vs2-pi/3
Vg= int2g(f59,a,b,c,d,20,20) %Gauss-Legendre
error=Vg-pi/3
Va= int2a(f59,a,b,c,d,0.0001,100) %Simpson-adaptive_smpson
error=Va-pi/3
