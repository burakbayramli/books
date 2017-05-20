t=0:0.05:5;% … used for continuation in the following line
y2=0.159*exp(t)+exp(-0.5*t).*(-0.659*cos(0.886*t)+ 0.013*sin(0.886*t))+0.5*cos(t)-0.5*sin(t);
y1=1/6*exp(t)-1/2*sin(t)+ 1/2*cos(t)-2/3*exp(-1/2*t).*cos(1/2*3^(1/2)*t);
plot(t,y1,'*',t,y2,'o')
title('Matlab and Analytical solution')
xlabel('t in seconds');
