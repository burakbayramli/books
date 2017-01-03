function Xw=CtFT1(x,Dt,w)
x_ejkwt=inline([x '(t).*exp(-j*w*t)'],'t','w');
Xw=trpzds_par(x_ejkwt,-Dt,Dt,1000,w);
%Xw=quad(x_ejkwt,-Dt,Dt,[],0,w); 
