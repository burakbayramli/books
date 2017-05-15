% Coiflet1 phi(t), psi(t)

%coefficients

h0=[-0.051429728471,0.238929728471,0.602859456942,0.272140543058...
,-0.051429972847,-0.011070271529]*sqrt(2);

Ns=128; %number of function samples

%scaling function
%using cascade algorithm
M=length(h0);
hN=sqrt(2)*h0;
phi=[ones(1,3*M*Ns),0]/(3*M); %initial iteration
%upsample hN, inserting Ns-1 zeros between samples
hup=[hN;zeros(Ns-1,M)];
hup=hup(1:(Ns*M));
%iteration
for nn=0:12,
   aux=conv(hup,phi);
   phi=aux(1:2:length(aux)); %downsampling by 2  
end

%wavelet
%the h1(n) coefficients
h1=fliplr(h0); h1(1:2:end)=-h1(1:2:end);
%the wavelet psi(t), using definition
%upsample
hN=sqrt(2)*h1;
h1up=[hN;zeros(Ns-1,M)];
h1up=h1up(1:Ns*M-1);
%downsample by 2
aux=conv(h1up,phi); 
psi=aux(1:2:length(aux));

%display
subplot(1,2,1)
phi=phi(1:(M-1)*Ns); %the supported part
t=(1:length(phi))/Ns;
plot(t,phi,'k'); %plots the scaling function
axis([0 max(t) 1.2*min(phi) 1.2*max(phi)]);
title('Coiflet1 scaling f.'); xlabel('t');

subplot(1,2,2)
psi=psi(1:(M-1)*Ns); %the supported part
t=(1:length(psi))/Ns;
plot(t,psi,'k'); %plots the wavelet
axis([0 max(t) 1.2*min(psi) 1.2*max(psi)]);
title('wavelet'); xlabel('t');


