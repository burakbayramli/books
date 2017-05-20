R=20*pi;
L=1;
w=0:0.1:100*pi;
T=R./(R+j*w*L);
Mag_T=abs(T);
plot(w,Mag_T);
xlabel('w in rad');
ylabel('Magnitude of the transfer function');
% press the mouse 3 times to get values on the graph
[x,y]=ginput(3)
