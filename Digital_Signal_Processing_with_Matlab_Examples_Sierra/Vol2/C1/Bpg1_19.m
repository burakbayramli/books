%Frequency bands of the cosine modulated filter bank
M=8; %number of channels
N=80; %prototype order
wc0=0.074813; %cut-off freq. (prototype)
beta=7.8573; %beta (prototype)
h=fir1(N,wc0,kaiser(N+1,beta)); %the prototype filter

mxH=zeros(M,N+1); %matrix of filter coeffs

aux1=pi/M; aux2=pi/4; axN=(N+1)/2;

for nk=0:M-1,
   for nj=0:N,
      mxH(nk+1,nj+1)=2*h(nj+1)*cos(aux1*(nk+0.5)*(nj+0.5-axN)+aux2*(-1^nk));
   end;
end;   

figure(1)
wiv=pi/512;
w=0:wiv:pi-wiv;
G=freqz(mxH(1,:),1);
plot(w,abs(G),'k'); grid; hold on;
axis([0 pi 0 1.2]);
title('Frequency response of the filters');
xlabel('w');

for nk=2:M,
 G=freqz(mxH(nk,:),1);
 plot(w,abs(G),'k'); 
end; 
 
 
