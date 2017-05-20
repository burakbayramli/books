%The range for wt from 0 to 3*pi with 0.05 increment
wt=0:0.05:3*pi;
v=100*cos(wt);
i=80*cos(wt-60*pi/180);
%when we multiply element by element we use the . before *
p=v.*i;
subplot(3,1,1), plot(wt,v), grid;
ylabel('v(t)'),
subplot(3,1,2), plot(wt,i), grid;
ylabel('i(t)'),
subplot(3,1,3), plot(wt,p), grid;
ylabel('p(t)'), xlabel('wt in rad'); 
