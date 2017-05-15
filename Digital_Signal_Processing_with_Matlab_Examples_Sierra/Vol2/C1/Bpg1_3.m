% Modulated analysis and synthesis
fs=20; %sampling frequency
tiv=1/fs; %sampling period
Ns=120; %number of samples
t=0:tiv:((Ns-1)*tiv); %time vector, 
%signal components
y0=cos(2*2*pi*t); 
y1=cos(6*2*pi*t);
y2=cos(10*2*pi*t);
%added signal
y=(1*y0)+(4*y1)+(3*y2);

%prototype filters (Kaiser, FIR)
fc=3/(fs/2); %cut-off frequency at 3 Hz
L=50;beta=5;
hw=kaiser(L+1,beta); %Kaiser window
Hnum=fir1(L,fc,hw); %FIR coeffs
Hden=[1]; %denominator
Fnum=Hnum;
Fden=Hden;

%modulations for 3 filters
m0=y.*exp(-j*(2*pi*0*t));
m1=y.*exp(-j*(2*pi*4*t));
m2=y.*exp(-j*(2*pi*8*t));

%low-pass filtering and decimation
a0=filter(Hnum,Hden,m0); b0=a0(1:3:Ns);
a1=filter(Hnum,Hden,m1); b1=a1(1:3:Ns);
a2=filter(Hnum,Hden,m2); b2=a2(1:3:Ns);

%upsampling
c0=zeros(1,Ns);c1=zeros(1,Ns);c2=zeros(1,Ns);  
c0(1:3:Ns)=b0(1:end);
c1(1:3:Ns)=b1(1:end);
c2(1:3:Ns)=b2(1:end);

%second low-pass filtering
M=3;
d0=M*filter(Fnum,Fden,c0);
d1=M*filter(Fnum,Fden,c1); 
d2=M*filter(Fnum,Fden,c2); 


tp=t-((50)*tiv); %delay compensation
%demodulations for 3 filters
dm0=d0.*exp(j*(2*pi*0*tp));
dm1=d1.*exp(j*(2*pi*4*tp));
dm2=d2.*exp(j*(2*pi*8*tp));

%output
x=dm0+dm1+dm2;

%display
figure(1)
plot(t,real(y),'k');
title('the input signal');
axis([0 6 -9 9]);

figure(2)
ff=logspace(0,1);
G=freqz(Hnum,Hden,ff,fs);
semilogx(ff,abs(G),'k'); grid;
title('Frequency response of the prototype filter');
xlabel('Hz');

figure(3)
subplot(1,3,1)
plot(t,real(m0));
title('ch.1, modulated input');
axis([0 6 -9 9]);
subplot(1,3,2)
plot(t,real(m1));
title('ch.2, modulated input');
axis([0 6 -9 9]);
subplot(1,3,3)
plot(t,real(m2));
title('ch.3, modulated input');
axis([0 6 -9 9]);

figure(4)
subplot(1,3,1)
plot(t,real(a0));
title('out.1, analysis filter');
axis([0 6 -6 6]);
subplot(1,3,2)
plot(t,real(a1));
title('out.2, analysis filter ');
axis([0 6 -6 6]);
subplot(1,3,3)
plot(t,real(a2));
title('out.3, analysis filter');
axis([0 6 -6 6]);

figure(5)
subplot(1,3,1)
plot(t(1:3:Ns),real(b0));
title('decimated 1');
axis([0 6 -6 6]);
subplot(1,3,2)
plot(t(1:3:Ns),real(b1));
title('decimated 2');
axis([0 6 -6 6]);
subplot(1,3,3)
plot(t(1:3:Ns),real(b2));
title('decimated 3');
axis([0 6 -6 6]);

figure(6)
subplot(1,3,1)
plot(t,real(c0));
title('upsampled 1');
axis([0 6 -6 6]);
subplot(1,3,2)
plot(t,real(c1));
title('upsampled 2');
axis([0 6 -6 6]);
subplot(1,3,3)
plot(t,real(c2));
title('upsampled 3');
axis([0 6 -6 6]);

figure(7)
subplot(1,3,1)
plot(t,real(d0));
title('out.1, synthesis filter');
axis([0 6 -6 6]);
subplot(1,3,2)
plot(t,real(d1));
title('out.2, synthesis filter');
axis([0 6 -6 6]);
subplot(1,3,3)
plot(t,real(d2));
title('out.3, synthesis filter');
axis([0 6 -6 6]);

figure(8)
subplot(1,3,1)
plot(t,real(dm0));
title('out.1, demodulation');
axis([0 6 -6 6]);
subplot(1,3,2)
plot(t,real(dm1));
title('out.2, demodulation');
axis([0 6 -6 6]);
subplot(1,3,3)
plot(t,real(dm2));
title('out.3, demodulation');
axis([0 6 -6 6]);

figure(9)
plot(t,real(x),'k');
title('the output signal');
axis([0 6 -9 9]);
