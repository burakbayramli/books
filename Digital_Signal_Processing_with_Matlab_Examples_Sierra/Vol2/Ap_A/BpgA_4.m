% Compute Daubechies phi(t), psi(t)
% for L=6,10,14..26

for MM=2:2:12,
K=MM+1;
a=1; p=1; q=1;
h=[1 1];
M=2*K; %length of the filter

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
subplot(6,2,(MM)-1)
phi=phi(1:(M-1)*Ns); %the supported part
t=(1:length(phi))/Ns;
plot(t,phi,'k'); %plots the scaling function
axis([0 max(t) 1.2*min(phi) 1.2*max(phi)]);

subplot(6,2,(MM))
psi=psi(1:(M-1)*Ns); %the supported part
plot(t,psi,'k'); %plots the wavelet
axis([0 max(t) 1.2*min(psi) 1.2*max(psi)]);

end;

