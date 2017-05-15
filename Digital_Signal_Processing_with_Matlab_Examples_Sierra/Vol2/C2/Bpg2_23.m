% Compute Daubechies h0(n), h1(n), phi(t), psi(t)
% for M=4 (length of the filter)
a=1; p=1; q=1;
h=[1 1];
M=4; %length of the filter
K=M/2;

% the h0(n) coefficients
for nn=1:K-1,
   h=conv(h,[1,1]);
   a=-a*0.25*(nn+K-1)/nn;
   p=conv(p,[1,-2,1]);
   q=[0 q 0] + a*p;
end;
q=sort(roots(q));
aux=real(poly(q(1:K-1)));
h=conv(h,aux);    
h0=(h*sqrt(2))/sum(h); %normalization
H0=fft(h0,512); %LP filter frequency response

%the scaling function phi(t), using cascade algorithm
Ns=128; %number of fi samples
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

%the h1(n) coefficients
h1=fliplr(h0); h1(1:2:end)=-h1(1:2:end);
H1=fft(h1,512); %HP filter frequency response
%the wavelet psi(t), using definition
%upsample by K
hN=-sqrt(2)*h1;
h1up=[hN;zeros(Ns-1,M)];
h1up=h1up(1:Ns*M-1);
%downsample by 2
aux=conv(h1up,phi); 
psi=aux(1:2:length(aux));

%display
subplot(2,2,1)
phi=phi(1:(M-1)*Ns); %the supported part
t=(1:length(phi))/Ns;
plot(t,phi,'k'); %plots the scaling function
axis([0 max(t) 1.2*min(phi) 1.2*max(phi)]);
title('Daubechies 4 scaling f.'); xlabel('t');

subplot(2,2,2)
psi=psi(1:(M-1)*Ns); %the supported part
plot(t,psi,'k'); %plots the wavelet
axis([0 max(t) 1.2*min(psi) 1.2*max(psi)]);
title('wavelet'); xlabel('t');

w=0:(2*pi/511):pi;
subplot(2,2,3)
plot(w,abs(H0(1:256)),'k');
axis([0 pi 0 2]);
title('|H0(w)|');xlabel('w');

subplot(2,2,4)
plot(w,abs(H1(1:256)),'k');
axis([0 pi 0 2]);
title('|H1(w)|');xlabel('w');

