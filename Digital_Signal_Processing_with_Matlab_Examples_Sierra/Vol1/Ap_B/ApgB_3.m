% Comparison of group delay of 5 filters
wc=10; %desired cut-off frequency
N=5; %order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[num,den]=butter(N,wc,'s'); %analog Butterworth filter
w=logspace(0,2,500); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
ph=angle(G); ph=unwrap(ph); %phase
npp=length(w); gd=zeros(npp,1);
for np=2:npp,
   gd(np)=(ph(np)-ph(np-1))/(w(np)-w(np-1));
end;
semilogx(w(2:npp),-gd(2:npp),'k'); %plots group delay
hold on;
axis([1 100 0 2]);
grid;
ylabel('group delay'); xlabel('rad/s'); title('comparison of group delay of the filters');

[num,den]=cheby1(N,Rp,wc,'s'); %analog Chebyshev 1 filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); ph=unwrap(ph);%phase
for np=2:npp,
   gd(np)=(ph(np)-ph(np-1))/(w(np)-w(np-1));
end;
semilogx(w(2:npp),-gd(2:npp),'r'); %plots group delay

[num,den]=cheby2(N,Rs,wc,'s'); %analog Chebyshev 2 filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); ph=unwrap(ph);%phase
for np=2:npp,
   gd(np)=(ph(np)-ph(np-1))/(w(np)-w(np-1));
   if gd(np)>1, gd(np)=gd(np-1); end; %elimination of discontinuity
end;
semilogx(w(2:npp),-gd(2:npp),'g'); %plots group delay

[num,den]=ellip(N,Rp,Rs,wc,'s'); %analog elliptic filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); ph=unwrap(ph);%phase
for np=2:npp,
   gd(np)=(ph(np)-ph(np-1))/(w(np)-w(np-1));
   if gd(np)>1, gd(np)=gd(np-1); end; %elimination of discontinuity
end;
semilogx(w(2:npp),-gd(2:npp),'b'); %plots group delay

[num,den]=besself(N,wc); %analog Bessel filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G);ph=unwrap(ph); %phase
for np=2:npp,
   gd(np)=(ph(np)-ph(np-1))/(w(np)-w(np-1));
end;
semilogx(w(2:npp),-gd(2:npp),'m'); %plots group delay
