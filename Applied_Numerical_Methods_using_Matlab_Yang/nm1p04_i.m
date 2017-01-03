%nm1p04: to plot a stratigraphic structure
clear, clf
x=[0.1 .. .. . ];
y=[0.5 .. .. . ];
Z=[410 390 .. .. .. .. ];
[X,Y]=meshgrid(x,y);
subplot(221), mesh(X,Y,500-Z)
subplot(222), surf(X,Y,500-Z)
subplot(223), meshc(X,Y,500-Z)
subplot(224), meshz(X,Y,500-Z)
pause
for k=0:7
   Az=-12.5*k; El=10*k; Azr=Az*pi/180; Elr=El*pi/180;
   subplot(221), view(Az,El)
   subplot(222), 
   k, view([sin(Azr),-cos(Azr),tan(Elr)]), pause %pause(1)
end
