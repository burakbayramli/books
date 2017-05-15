% Dual-tree complex wavelet example

%coefficients (scaling functions)

ah0=[0.00419528584157, -0.03976408134143, -0.08807084231507,...
     0.28789890325798, 0.80289644768232, 0.50734324828341,...
     -0.04438514804476, -0.05179712664076, 0.03247103802248,...
     0.00342583762736];
sh0=[0.00051763584333, -0.00016716564000, -0.09187942035452,...
     0.02408482114448, 0.61837942541527, 0.75480699639212,...
     0.17400853530401, -0.09044673462008, 0.00608060497845,...
     0.01882886391002, 0];
    
Ns=128; %number of function samples

%first scaling function
%using cascade algorithm
Ma=length(ah0);
ah0=2*ah0/sum(ah0); %normalization
aphi=[ones(1,3*Ma*Ns),0]/(3*Ma); %initial iteration
%upsample hN, inserting Ns-1 zeros between samples
hup=[ah0;zeros(Ns-1,Ma)];
hup=hup(1:(Ns*Ma));
%iteration
for nn=0:12,
   aux=conv(hup,aphi);
   aphi=aux(1:2:length(aux)); %downsampling by 2  
end

%second scaling function
%using cascade algorithm
Ms=length(sh0);
sh0=2*sh0/sum(sh0); %normalization
sphi=[ones(1,3*Ms*Ns),0]/(3*Ms); %initial iteration
%upsample hN, inserting Ns-1 zeros between samples
hup=[sh0;zeros(Ns-1,Ms)];
hup=hup(1:(Ns*Ms));
%iteration
for nn=0:12,
   aux=conv(hup,sphi);
   sphi=aux(1:2:length(aux)); %downsampling by 2  
end

%first wavelet
%the ah1(n) coefficients
ah1=fliplr(sh0); ah1(1:2:end)=-ah1(1:2:end);
%the wavelet psi(t), using definition
%upsample
hN=sqrt(2)*ah1;
h1up=[hN;zeros(Ns-1,Ms)];
h1up=h1up(1:Ns*Ms-1);
%downsample by 2
aux=conv(h1up,aphi); 
ax=aux(1:2:length(aux));
apsi=fliplr(ax);

%second wavelet
%the sh1(n) coefficients
sh1=fliplr(ah0);sh1(1:2:end)=-sh1(1:2:end);
%the wavelet psi(t), using definition
%upsample
hN=-sqrt(2)*sh1;
h1up=[hN;zeros(Ns-1,Ma)];
h1up=h1up(1:Ns*Ma-1);
%downsample by 2
aux=conv(h1up,sphi); 
ax=aux(1:2:length(aux));
spsi=fliplr(ax);

%spectrum of the complex wavelet
FA=fftshift(fft(apsi));
FS=fftshift(fft(spsi));
S=abs(FA+(i*FS));
S=S/max(S); %normalize

%display
figure(1)
su=1280;
ti=256;
t=(ti:su)/Ns;
plot(t,apsi(ti:su),'r'); hold on; %plots the wavelet
plot(t,spsi(ti:su),'k'); %plots the other wavelet
axis([ti/Ns max(t) -2.5 2.5]);
title('the wavelets'); xlabel('t');

figure(2)
w=-pi:(2*pi/(length(S)-1)):pi;
rg=600:740; %horizontal zoom
plot(w(rg),S(rg),'k');
axis([-0.4 0.4 0 1.1]);
title('spectrum');



