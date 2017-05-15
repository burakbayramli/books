% CDF 9/7 phi(t), psi(t)

%coefficients
ah0=[-0.045635881557,-0.028771763114,0.295635881557,0.557543526229...
,0.295635881557,-0.028771763114,-0.045635881557];

sh0=[0.026748757411,-0.016864118443,-0.078223266529,0.266864118443...
,0.602949018236,0.266864118443,-0.078223266529,-0.016864118443,0.026748757411];

Ns=128; %number of function samples

%analysis scaling function
%using cascade algorithm
Ma=length(ah0);
ah0=2*ah0/sum(ah0);
aphi=[ones(1,3*Ma*Ns),0]/(3*Ma); %initial iteration
%upsample hN, inserting Ns-1 zeros between samples
hup=[ah0;zeros(Ns-1,Ma)];
hup=hup(1:(Ns*Ma));
%iteration
for nn=0:12,
   aux=conv(hup,aphi);
   aphi=aux(1:2:length(aux)); %downsampling by 2  
end

%synthesis scaling function
%using cascade algorithm
Ms=length(sh0);
sh0=2*sh0/sum(sh0);
sphi=[ones(1,3*Ms*Ns),0]/(3*Ms); %initial iteration
%upsample hN, inserting Ns-1 zeros between samples
hup=[sh0;zeros(Ns-1,Ms)];
hup=hup(1:(Ns*Ms));
%iteration
for nn=0:12,
   aux=conv(hup,sphi);
   sphi=aux(1:2:length(aux)); %downsampling by 2  
end


%analysis wavelet
%the ah1(n) coefficients
ah1=fliplr(sh0); ah1(1:2:end)=-ah1(1:2:end);
%the wavelet psi(t), using definition
%upsample
hN=-sqrt(2)*ah1;
h1up=[hN;zeros(Ns-1,Ms)];
h1up=h1up(1:Ns*Ms-1);
%downsample by 2
aux=conv(h1up,aphi); 
apsi=aux(1:2:length(aux));


%synthesis wavelet
%the sh1(n) coefficients
sh1=fliplr(ah0); sh1(1:2:end)=-sh1(1:2:end);
%the wavelet psi(t), using definition
%upsample
hN=sqrt(2)*sh1;
h1up=[hN;zeros(Ns-1,Ma)];
h1up=h1up(1:Ns*Ma-1);
%downsample by 2
aux=conv(h1up,sphi); 
spsi=aux(1:2:length(aux));

%display
subplot(2,2,1)
aphi=aphi(1:(Ma-1)*Ns); %the supported part
t=(1:length(aphi))/Ns;
plot(t,aphi,'k'); %plots the scaling function
axis([0 max(t) 1.2*min(aphi) 1.2*max(aphi)]);
title('CDF 9/7 analysis scaling f.'); xlabel('t');

subplot(2,2,3)
su=round(0.9*length(apsi));
t=(1:su)/Ns;
plot(t,apsi(1:su),'k'); %plots the wavelet
axis([0 max(t) 1.2*min(apsi) 1.2*max(apsi)]);
title('analysis wavelet'); xlabel('t');

subplot(2,2,2)
sphi=sphi(1:(Ms-1)*Ns); %the supported part
t=(1:length(sphi))/Ns;
plot(t,sphi,'k'); %plots the scaling function
axis([0 max(t) 1.2*min(sphi) 1.2*max(sphi)]);
title('synthesis scaling f.'); xlabel('t');

subplot(2,2,4)
su=round(0.9*length(spsi));
t=(1:su)/Ns;
plot(t,spsi(1:su),'k'); %plots the wavelet
axis([0 max(t) 1.2*min(spsi) 1.2*max(spsi)]);
title('synthesis wavelet'); xlabel('t');


